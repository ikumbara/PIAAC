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

# ----------------------------
# Rubin's Rule Implementation
# ----------------------------

#' Apply Rubin's Rule to combine estimates from multiple imputations
#' 
#' @param estimates Vector of point estimates from each imputation
#' @param variances Vector of variances from each imputation
#' @param alpha Significance level (default 0.05 for 95% CI)
#' @return List containing pooled estimate, variance, standard error, t-statistic, p-value, and confidence interval
rubins_rule <- function(estimates, variances, alpha = 0.05) {
  m <- length(estimates)  # Number of imputations
  
  # Point estimate: average of estimates
  Q_bar <- mean(estimates, na.rm = TRUE)
  
  # Within-imputation variance: average of variances
  U_bar <- mean(variances, na.rm = TRUE)
  
  # Between-imputation variance
  B <- var(estimates, na.rm = TRUE)
  
  # Total variance
  T_total <- U_bar + (1 + 1/m) * B
  
  # Standard error
  se <- sqrt(T_total)
  
  # Degrees of freedom for t-distribution
  # Using the formula from Rubin (1987)
  r <- (1 + 1/m) * B / U_bar
  df <- (m - 1) * (1 + 1/r)^2
  
  # t-statistic for testing H0: parameter = 0
  t_stat <- Q_bar / se
  
  # p-value (two-tailed)
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  # Confidence interval
  t_crit <- qt(1 - alpha/2, df = df)
  ci_lower <- Q_bar - t_crit * se
  ci_upper <- Q_bar + t_crit * se
  
  return(list(
    estimate = Q_bar,
    variance = T_total,
    se = se,
    df = df,
    t_statistic = t_stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    within_var = U_bar,
    between_var = B,
    total_var = T_total
  ))
}

#' Pool regression results using Rubin's Rule
#' 
#' @param results_list List of regression results (e.g., from fixest)
#' @return Data frame with pooled estimates for each coefficient
pool_regression_results <- function(results_list) {
  # Extract coefficient names from first model
  coef_names <- names(coef(results_list[[1]]))
  
  # Initialize results dataframe
  pooled_results <- data.frame(
    term = coef_names,
    estimate = NA,
    se = NA,
    t_statistic = NA,
    p_value = NA,
    ci_lower = NA,
    ci_upper = NA,
    df = NA,
    stringsAsFactors = FALSE
  )
  
  # Apply Rubin's Rule to each coefficient
  for (i in seq_along(coef_names)) {
    coef_name <- coef_names[i]
    
    # Extract estimates and variances for this coefficient across all imputations
    estimates <- sapply(results_list, function(x) coef(x)[coef_name])
    variances <- sapply(results_list, function(x) vcov(x)[coef_name, coef_name])
    
    # Apply Rubin's Rule
    pooled <- rubins_rule(estimates, variances)
    
    # Store results
    pooled_results[i, "estimate"] <- pooled$estimate
    pooled_results[i, "se"] <- pooled$se
    pooled_results[i, "t_statistic"] <- pooled$t_statistic
    pooled_results[i, "p_value"] <- pooled$p_value
    pooled_results[i, "ci_lower"] <- pooled$ci_lower
    pooled_results[i, "ci_upper"] <- pooled$ci_upper
    pooled_results[i, "df"] <- pooled$df
  }
  
  return(pooled_results)
}

# ----------------------------
# Apply Rubin's Rule to all regression results
# ----------------------------

# Pool numeracy results
pooled_num_2012 <- pool_regression_results(results_num_2012)
pooled_num_2017 <- pool_regression_results(results_num_2017)

# Pool literacy results  
pooled_lit_2012 <- pool_regression_results(results_lit_2012)
pooled_lit_2017 <- pool_regression_results(results_lit_2017)

# ----------------------------
# Display pooled results
# ----------------------------

print("=== POOLED NUMERACY RESULTS 2012 ===")
print(pooled_num_2012)

print("=== POOLED NUMERACY RESULTS 2017 ===")
print(pooled_num_2017)

print("=== POOLED LITERACY RESULTS 2012 ===")
print(pooled_lit_2012)

print("=== POOLED LITERACY RESULTS 2017 ===")
print(pooled_lit_2017)

# ----------------------------
# Create summary function for easier interpretation
# ----------------------------

#' Create a formatted summary of pooled results
#' 
#' @param pooled_results Data frame from pool_regression_results()
#' @param title Title for the summary
format_pooled_summary <- function(pooled_results, title) {
  cat("\n", paste(rep("=", nchar(title) + 4), collapse = ""), "\n")
  cat(" ", title, "\n")
  cat(paste(rep("=", nchar(title) + 4), collapse = ""), "\n")
  
  for (i in 1:nrow(pooled_results)) {
    row <- pooled_results[i, ]
    significance <- ifelse(row$p_value < 0.001, "***",
                          ifelse(row$p_value < 0.01, "**",
                                ifelse(row$p_value < 0.05, "*",
                                      ifelse(row$p_value < 0.1, ".", ""))))
    
    cat(sprintf("%-20s: %8.4f (%8.4f) t=%-6.2f p=%6.4f %s [%6.4f, %6.4f]\n",
                row$term, row$estimate, row$se, row$t_statistic, 
                row$p_value, significance, row$ci_lower, row$ci_upper))
  }
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

# Display formatted summaries
format_pooled_summary(pooled_num_2012, "NUMERACY 2012 - POOLED RESULTS")
format_pooled_summary(pooled_num_2017, "NUMERACY 2017 - POOLED RESULTS") 
format_pooled_summary(pooled_lit_2012, "LITERACY 2012 - POOLED RESULTS")
format_pooled_summary(pooled_lit_2017, "LITERACY 2017 - POOLED RESULTS")
