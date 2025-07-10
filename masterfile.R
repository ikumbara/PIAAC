path <- normalizePath("C:/Users/bupsi/OneDrive/Masaüstü/M12 Data/do files US", mustWork = FALSE)

required_packages <- c("assertthat","cobalt","stargazer","mlr3","mlr3learners","DoubleML","fixest","caret","grf", "MatchIt","matrixStats",
                       "janitor", "readr", "dplyr", "ranger", "lme4", "Hmisc",
                       "summarytools", "modelsummary", "labelled", "randomForest", "tidyverse",
                       "ggplot2", "mice", "broom", "dotwhisker", "xgboost", "jtools")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

#Source R scripts for merging and preparing US data (2012 & 2017 only)
merge_file <- file.path(path, "data merge.R")
prep_file <- file.path(path, "data prep.R")
PSM_file <- file.path(path, "PSM.R")
mah_file <- file.path(path, "mah.R")
ML_file <- file.path(path, "ML.R")

if (file.exists(merge_file)) source(merge_file) else stop("Merge script not found.")
if (file.exists(prep_file)) source(prep_file) else stop("Prep script not found.")
if (file.exists(PSM_file)) source(PSM_file) else stop("PSM script not found.")
if (file.exists(mah_file)) source(mah_file) else stop("mah script not found.")
if (file.exists(ML_file)) source(ML_file) else stop("Machine Learning script not found.")
