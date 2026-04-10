
packages = c(
	"tidyverse", "janitor", "haven", "zip",
	"stringi", "here")

check_packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
setwd(here())
rm(list = ls())

#=====================================================================
#--------------------- DATA PRIVACY FUNCTION -------------------------
#=====================================================================
# This file contains strict privacy protocols.
# It is locked to prevent accidental modification during runtime.

ensure_data_privacy <- function(df, threshold = 200) {
  n_start <- nrow(df)
  
  df_clean <- df %>%
    # 1. Top-coding Income: Cap at 99th percentile to hide extreme outliers
    group_by(year) %>%
    mutate(
      income_cap = quantile(i524e1, 0.99, na.rm = TRUE),
      i524e1 = if_else(i524e1 > income_cap, income_cap, i524e1)
    ) %>%
    ungroup() %>%
    # 2. K-Anonymity: Ensure at least 200 individuals exist for every combination
    # of District (ubigeo), Sex, and Age.
    group_by(ubigeo, sex, age_categories) %>%
    filter(n() >= threshold) %>%
    ungroup() %>%
    select(-income_cap)
    
  n_dropped <- n_start - nrow(df_clean)
  if(n_dropped > 0) message(paste0("Privacy Check: Dropped ", n_dropped, " rows (Threshold: n < ", threshold, ")."))
  
  return(df_clean)
}
