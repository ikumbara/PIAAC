library(tidyverse)
library(MatchIt)
library(cobalt)
library(Hmisc)
library(assertthat)
library(fixest)
library(broom)
library(dplyr)
library(readr)
library(ggplot2)

# ----------------------------
# Logging function for dropped rows
# ----------------------------
log_drop_stats <- function(before, after, label) {
  message(sprintf("[%s] Rows before: %d | after: %d | dropped: %d", label, before, after, before - after))
}

# ----------------------------
# Load data
# ----------------------------
piaac2012 <- read_csv(file.path(path, "data_2012_clean.csv"), show_col_types = FALSE)
piaac2017 <- read_csv(file.path(path, "data_2017_clean.csv"), show_col_types = FALSE)

# ----------------------------
# Age band mapping
# ----------------------------
ageband_map <- c("16–19"=1, "20–24"=2, "25–29"=3, "30–34"=4, "35–39"=5, 
                 "40–44"=6, "45–49"=7, "50–54"=8, "55–59"=9, "60–65"=10, 
                 "66–70"=11, "71 plus"=12)

piaac2012 <- piaac2012 %>%
  mutate(
    ageband_num = ageband_map[ageband],
    cohort_match_band = ageband_num,
    age_mid = (as.numeric(str_sub(ageband, 1, 2)) + as.numeric(str_sub(ageband, 4, 5))) / 2
  )

piaac2017 <- piaac2017 %>%
  mutate(
    ageband_num = ageband_map[ageband],
    cohort_match_band = ageband_num - 1,
    age_mid = if_else(ageband_num == 12, 72,
                      (as.numeric(str_sub(ageband, 1, 2)) + as.numeric(str_sub(ageband, 4, 5))) / 2)
  )

# ----------------------------
# Common transformation
# ----------------------------
transform_common <- function(df, year_label) {
  n0 <- nrow(df)
  df <- df %>%
    mutate(
      hstat = as.factor(hstat),
      female = as.factor(female),
      nat_eng = as.factor(nat_eng),
      edcat3 = as.factor(edcat3),
      #region = factor(region) %>% droplevels(),
      isco1c = as.factor(case_when(
        isco1c %in% c(9995, 9996, 9997, 9998, 9999) ~ "None",
        TRUE ~ as.character(floor(as.numeric(isco1c)))
      ))
    ) %>%
    filter(
      #!is.na(region),
      !(litstatus == 2 & numstatus == 2),
      !isco1c %in% c("None", "R", "V", ".", "D", "N", ""),
      !is.na(isco1c),
      !hstat %in% c("None", "R", "V", ".", "D", "N", ""),
      !is.na(hstat),
      employed == 1
    )
  log_drop_stats(n0, nrow(df), paste("transform_common", year_label))
  #assert_that(!any(is.na(df$region)))
  assert_that(!any(is.na(df$isco1c)))
  assert_that(!any(is.na(df$hstat)))
  return(df)
}

piaac2012 <- transform_common(piaac2012, "2012")
piaac2017 <- transform_common(piaac2017, "2017")


# ----------------------------
# Skill use items
# ----------------------------
lit_letters <- letters[1:6]
num_letters <- c(letters[2:4], letters[6:8])
work_lit <- paste0("g_q01", lit_letters)
home_lit <- paste0("h_q01", lit_letters)
work_num <- paste0("g_q03", num_letters)
home_num <- paste0("h_q03", num_letters)
all_items <- c(work_lit, home_lit, work_num, home_num)

# ----------------------------
# Create new binary variables (ending in _bin), preserve original g/h items
# ----------------------------
to_monthly_binary <- function(df, items) {
  for (item in items) {
    bin_var <- paste0("d3_", item)
    df[[bin_var]] <- ifelse(!is.na(df[[item]]) & as.numeric(df[[item]]) > 2, 1,
                            ifelse(!is.na(df[[item]]), 0, NA_real_))
  }
  return(df)
}

# Apply to both years using the defined skill use items
piaac2012 <- to_monthly_binary(piaac2012, all_items)
piaac2017 <- to_monthly_binary(piaac2017, all_items)

# ----------------------------
# Compute skill use indices
# ----------------------------
skill_use_indices_bin <- function(df, year_label = NULL) {
  initial_n <- nrow(df)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      md3_lit_n_raw = mean(c_across(all_of(paste0("d3_", work_lit))), na.rm = TRUE),
      md3_num_n_raw = mean(c_across(all_of(paste0("d3_", work_num))), na.rm = TRUE),
      md3_lit_ho_n_raw = mean(c_across(all_of(paste0("d3_", home_lit))), na.rm = TRUE),
      md3_num_ho_n_raw = mean(c_across(all_of(paste0("d3_", home_num))), na.rm = TRUE),
      md3_lit_a_raw = round(mean(c(md3_lit_n_raw, md3_lit_ho_n_raw), na.rm = TRUE), 6),
      md3_num_a_raw = round(mean(c(md3_num_n_raw, md3_num_ho_n_raw), na.rm = TRUE), 6)
    ) %>%
    ungroup()
  
  skill_use_vars <- c("md3_lit_n_raw", "md3_num_n_raw", "md3_lit_ho_n_raw",
                      "md3_num_ho_n_raw", "md3_lit_a_raw", "md3_num_a_raw")
  df <- df %>% filter(if_any(all_of(skill_use_vars), ~ !is.na(.)))
  
  message(sprintf("[%s] Dropped %d rows due to missing skill use indices", 
                  year_label, initial_n - nrow(df)))
  
  for (v in skill_use_vars) {
    assertthat::assert_that(!all(is.na(df[[v]])))
  }
  return(df)
}

piaac2012 <- skill_use_indices_bin(piaac2012, "2012")
piaac2017 <- skill_use_indices_bin(piaac2017, "2017")

# ----------------------------
# Weighted median split
# ----------------------------
median_split <- function(df, weight_var = "spfwt0", prefix = "d_") {
  skill_vars <- c("md3_lit_n_raw", "md3_num_n_raw", "md3_lit_ho_n_raw", 
                  "md3_num_ho_n_raw", "md3_lit_a_raw", "md3_num_a_raw")
  for (var in skill_vars) {
    median_val <- Hmisc::wtd.quantile(df[[var]], weights = df[[weight_var]], probs = 0.5, na.rm = TRUE)
    binary_var <- paste0(prefix, var)
    df[[binary_var]] <- ifelse(!is.na(df[[var]]), ifelse(df[[var]] > median_val, 1, 0), NA)
    assertthat::assert_that(!all(is.na(df[[binary_var]])))
  }
  return(df)
}

piaac2012 <- median_split(piaac2012)
piaac2017 <- median_split(piaac2017)

# Scale (0–1) for a_md3 (skill use)
# ----------------------------
rescale_use_items <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      a_md3_lit_raw = mean(c_across(all_of(c(work_lit, home_lit))), na.rm = TRUE),
      a_md3_num_raw = mean(c_across(all_of(c(work_num, home_num))), na.rm = TRUE),
      a_md3_lit = (a_md3_lit_raw - 1) / 4,
      a_md3_num = (a_md3_num_raw - 1) / 4
    ) %>%
    ungroup()
}
piaac2012 <- rescale_use_items(piaac2012)
piaac2017 <- rescale_use_items(piaac2017)

# ----------------------------
# Standardize function
# ----------------------------
standardize_weighted <- function(x, w) {
  m <- weighted.mean(x, w, na.rm = TRUE)
  s <- sqrt(Hmisc::wtd.var(x, w, na.rm = TRUE))
  return((x - m) / s)
}

# PV + continuous
zc_vars <- c(paste0("pvnum", 1:10), paste0("pvlit", 1:10), 
             "md3_lit_n_raw", "md3_num_n_raw", "md3_lit_ho_n_raw", 
             "md3_num_ho_n_raw", "md3_lit_a_raw", "md3_num_a_raw", 
             "a_md3_lit", "a_md3_num")

for (v in zc_vars) {
  piaac2012[[paste0("zc_", v)]] <- standardize_weighted(piaac2012[[v]], piaac2012$spfwt0)
  piaac2017[[paste0("zc_", v)]] <- standardize_weighted(piaac2017[[v]], piaac2017$spfwt0)
}

# Binary
zb_vars <- grep("^d_", names(piaac2012), value = TRUE)
for (v in zb_vars) {
  piaac2012[[paste0("zb_", v)]] <- standardize_weighted(piaac2012[[v]], piaac2012$spfwt0)
  piaac2017[[paste0("zb_", v)]] <- standardize_weighted(piaac2017[[v]], piaac2017$spfwt0)
}

piaac2012 <- piaac2012 %>%
  mutate(
    white_coll = ifelse(as.numeric(as.character(isco1c)) < 5, 1, 0),
    college = ifelse(edcat3 == "Tertiary", 1, 0)
    )

piaac2017 <- piaac2017 %>%
  mutate(
    white_coll = ifelse(as.numeric(as.character(isco1c)) < 5, 1, 0),
    college = ifelse(edcat3 == "Tertiary", 1, 0)
    )

# Function for numeracy regression (uses md3_num_a_raw)
run_wls_num <- function(data, iter, weight_spec) {
  skill_var <- paste0("zc_pvnum", iter)
  formula <- as.formula(paste(
    skill_var,
    "~ age_mid + I(age_mid^2 / 1000) + md3_num_a_raw + female + college + white_coll"
  ))
  feols(formula, weights = weight_spec, data = data)
}

# Function for literacy regression (uses md3_lit_a_raw)
run_wls_lit <- function(data, iter, weight_spec) {
  skill_var <- paste0("zc_pvlit", iter)
  formula <- as.formula(paste(
    skill_var,
    "~ age_mid + I(age_mid^2 / 1000) + md3_lit_a_raw + female + college + white_coll"
  ))
  feols(formula, weights = weight_spec, data = data)
}

# Run regressions for both years and both skills
results_num_2012 <- lapply(1:10, function(i) run_wls_num(piaac2012, i, piaac2012$spfwt0))
results_num_2017 <- lapply(1:10, function(i) run_wls_num(piaac2017, i, piaac2017$spfwt0))

results_lit_2012 <- lapply(1:10, function(i) run_wls_lit(piaac2012, i, piaac2012$spfwt0))
results_lit_2017 <- lapply(1:10, function(i) run_wls_lit(piaac2017, i, piaac2017$spfwt0))
