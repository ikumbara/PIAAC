
library(readr)
library(dplyr)
library(janitor)

#DEFINE FUNCTIONS

#recode gender female 2 to 1 and male 1 to 0, rename female, later use to match
rec_gen <- function(data) {
  data %>%
    mutate(
      gender = case_when(
        gender_r == 1 ~ 0,
        gender_r == 2 ~ 1,
        TRUE ~ NA_real_
        ),
      female = gender
    )
}

#recode native speakers, later use to match
rec_nat <- function(data) {
  data %>%
    mutate(
      native = case_when(
        nativespeaker == 1 ~ 1,
        nativespeaker == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      nat_eng = native
    )
}

#age band and year function, later use to match
year_age <- function(data, year_val, ageband_col, year_col = "year") {
  data %>%
    mutate(
      year = year_val,
      ageband_raw = .[[ageband_col]],
      ageband = case_when(
        ageband_raw == 1 ~ "16–19",
        ageband_raw == 2 ~ "20–24",
        ageband_raw == 3 ~ "25–29",
        ageband_raw == 4 ~ "30–34",
        ageband_raw == 5 ~ "35–39",
        ageband_raw == 6 ~ "40–44",
        ageband_raw == 7 ~ "45–49",
        ageband_raw == 8 ~ "50–54",
        ageband_raw == 9 ~ "55–59",
        ageband_raw == 10 ~ "60–65",
        ageband_raw == 11 ~ "66–70",
        ageband_raw == 12 ~ "71 plus",
        TRUE ~ NA_character_
      )
    )
}

#transformation function for edcat6
rec_ed <- function(data) {
  data %>%
    mutate(
      edcat3 = case_when(
        edcat6 == "1" ~ "Lower secondary or less",
        edcat6 %in% c("2", "3") ~ "Upper & post-secondary",
        edcat6 %in% c("4", "5", "6") ~ "Tertiary",
        TRUE ~ NA_character_  #for values "", "99", NA
      )
    )
}

#transformation function for employment
rec_emp <- function(data) {
  data %>%
    mutate(
      employed = case_when(
        c_d05 == "1" ~ 1,
        c_d05 %in% c("2", "3", "4") ~ 0,
        TRUE ~ NA_real_
      )
    )
}

pv_scr <- function(data) {
  prefixes <- c("pvlit", "pvnum")
  for (prefix in prefixes) {
    for (i in 1:10) {
      varname <- paste0(prefix, i)
      data[[varname]] <- ifelse(data[[varname]] %in% c("N", "", "."), NA_character_, data[[varname]])
      data[[varname]] <- as.numeric(data[[varname]])
    }
  }
  return(data)
}

rec_usage_q1 <- function(data) {
  prefixes <- c("g_q01", "h_q01")
  for (prefix in prefixes) {
    for (i in letters[1:6]) {
      varname <- paste0(prefix, i)
      data[[varname]] <- ifelse(data[[varname]] %in% c("D", "N", "V", "R", "", "."), NA_character_, data[[varname]])
      data[[varname]] <- as.numeric(data[[varname]])
    }
  }
  return(data)
}

rec_usage_q3 <- function(data) {
  prefixes <- c("g_q03", "h_q03")
  for (prefix in prefixes) {
    for (i in c(letters[2:4], letters[6:8])) {
      varname <- paste0(prefix, i)
      data[[varname]] <- ifelse(data[[varname]] %in% c("D", "N", "V", "R", "", "."), NA_character_, data[[varname]])
      data[[varname]] <- as.numeric(data[[varname]])
    }
  }
  return(data)
}

#occupational grouping
rec_isco <- function(data) {
  data %>%
    mutate(
      isco1c = if_else(as.numeric(isco1c) <= 9999, as.numeric(isco1c), NA_real_),
      isco2c = if_else(as.numeric(isco2c) <= 9999, as.numeric(isco2c), NA_real_)
    )
}

#health grouping
rec_hel <- function(data) {
  data %>%
    mutate(
      hstat = as.numeric(i_q08),
    )
}

#recode region, later use to match
reg_us <- function(data){
  data %>%
    mutate(
      region_us = as.character(region_us),
      region = case_when(
        region_us == "1" ~ "Northeast",
        region_us == "2" ~ "Midwest",
        region_us == "3" ~ "South",
        region_us == "4" ~ "West",
        TRUE ~ NA_character_
      )
    )
}


#column selection
select_final_columns <- function(data) {
  data %>%
    select(
      seqid, year, spfwt0, female, edcat3, isco1c, isco2c, region, g_q01a, g_q01b,
      g_q01c, g_q01d, g_q01e, g_q01f, g_q03b, g_q03c, g_q03d,
      g_q03f, g_q03g, g_q03h, h_q01a, h_q01b, h_q01c, h_q01d, h_q01e,
      h_q01f, h_q03b, h_q03c, h_q03d, h_q03f, h_q03g, h_q03h,
      employed,  ageband, pvlit1, pvlit2, pvlit3, pvlit4, pvlit5, pvlit6, pvlit7,
      pvlit8, pvlit9, pvlit10, pvnum1, pvnum2, pvnum3, pvnum4, pvnum5, pvnum6, 
      pvnum7, pvnum8, pvnum9, pvnum10, litstatus, numstatus, hstat, nat_eng
      )
}

transform_piaac_data <- function(df, year_val, ageband_col) {
  df %>%
    rec_gen() %>%
    rec_nat() %>%
    rec_emp() %>%
    year_age(year_val, ageband_col) %>%
    rec_ed() %>%
    rec_isco() %>%
    pv_scr() %>%
    rec_usage_q1() %>%
    rec_usage_q3() %>%
    reg_us() %>%
    rec_hel() %>%
    select_final_columns()
}

#read and clean 2012
data_2012 <- read_csv(file.path(path, "prgusap1_2012.csv"), show_col_types = FALSE) %>%
  clean_names()

#read and clean 2017
data_2017 <- read_csv(file.path(path, "prgusap1_2017.csv"), show_col_types = FALSE) %>%
  clean_names()

#transform
data_2012_clean <- transform_piaac_data(data_2012, 2012, "ageg5lfs")
write_csv(data_2012_clean, file.path(path, "data_2012_clean.csv"))

data_2017_clean <- transform_piaac_data(data_2017, 2017, "ageg5lfsext")
write_csv(data_2017_clean, file.path(path, "data_2017_clean.csv"))
