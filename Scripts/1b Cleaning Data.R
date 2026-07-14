# ==============================================================================
# STEP 1d: Merge and Clean Datasets (Master Compilation)
# Economics Honors Thesis Data Pipeline
# ==============================================================================
# This script consolidates the three main processed data components:
#   1. Raw non-profit financial data (extracted from Form 990)
#   2. State-level GDP growth rates (BEA)
#   3. Organization industry classification sectors (IRS Activity/NTEE codes)
# It handles missing value imputations and cleans up variables to prepare a master
# analysis file.
# ==============================================================================

# Core Libraries
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

# ==============================================================================
# 1. Load Processed Datasets
# ==============================================================================
org_data <- readRDS("../Final_Data/final_master_data.rds")
org_data <- org_data |> mutate(YEAR = as.integer(Year))

has_gdp <- FALSE
if (file.exists("../Final_Data/gdp_data.rds")) {
  gdp_data <- readRDS("../Final_Data/gdp_data.rds") |> 
    mutate(YEAR = as.integer(YEAR))
  has_gdp <- TRUE
} else {
  message(">>> gdp_data.rds not found. GDP variables will not be joined.")
}

has_activity <- FALSE
if (file.exists("../Final_Data/activity_data.rds")) {
  activity_data <- readRDS("../Final_Data/activity_data.rds")
  has_activity <- TRUE
} else {
  message(">>> activity_data.rds not found. Industry activity classifications will not be joined.")
}

# ==============================================================================
# 2. Impute Missing Financial Values
# A missing value (NA) in Form 990 financial tables usually indicates that the
# organization had $0 for that category (e.g. no fundraising or membership dues).
# We replace these NAs with 0 to enable numerical calculations and regressions.
# ==============================================================================
columns_to_replace <- c(
  # Supply-side revenue categories
  "F9_08_REV_CONTR_FED_CAMP",
  "F9_08_REV_CONTR_MEMBSHIP_DUE",
  "F9_08_REV_CONTR_FUNDR_EVNT",
  "F9_08_REV_CONTR_RLTD_ORG",
  "F9_08_REV_CONTR_GOVT_GRANT",
  "F9_08_REV_CONTR_OTH",
  "F9_08_REV_CONTR_TOT",
  "F9_08_REV_PROG_TOT_TOT",
  "F9_08_REV_OTH_INVEST_INCOME_TOT",
  "F9_08_REV_OTH_INVEST_BOND_TOT",
  "F9_08_REV_OTH_ROY_TOT",
  "F9_08_REV_OTH_RENT_NET_TOT",
  "F9_08_REV_OTH_SALE_GAIN_NET_TOT",
  "F9_08_REV_OTH_FUNDR_NET_TOT",
  "F9_08_REV_OTH_GAMING_NET_TOT",
  "F9_08_REV_OTH_INV_NET_TOT",
  "F9_08_REV_MISC_TOT_TOT",
  "F9_08_REV_TOT_TOT",
  # Demand-side expense categories
  "F9_09_EXP_GRANT_US_ORG_TOT",
  "F9_09_EXP_GRANT_US_INDIV_TOT",
  "F9_09_EXP_GRANT_FRGN_TOT",
  "F9_09_EXP_TOT_TOT",
  "F9_09_EXP_TOT_PROG",
  "F9_09_EXP_TOT_MGMT",
  "F9_09_EXP_TOT_FUNDR"
)

# Impute NA values to 0 across the specified financial columns
org_data <- org_data |>
  mutate(across(
    .cols = any_of(columns_to_replace),
    .fns = ~ replace_na(.x, 0)
  ))

# ==============================================================================
# 3. Create State Abbreviation Lookup
# GDP data has full state names, while Form 990 lists postal codes.
# We map full names to abbreviations (e.g. "Alabama" -> "AL") to enable joins.
# ==============================================================================
state_lookup <- data.frame(
  state_name = state.name,
  state_abb = state.abb,
  stringsAsFactors = FALSE
)

# Add abbreviations to the GDP dataset
if (has_gdp) {
  gdp_data <- gdp_data |>
    left_join(
      state_lookup,
      by = c("state" = "state_name")
    )
}

# Sort the dataset by organization and year to verify panel sequencing
org_data <- org_data |>
  arrange(ORG_EIN, YEAR)

# ==============================================================================
# 4. Merge Datasets
# ==============================================================================

# Join 1: Attach state-level GDP variables to each organization based on Year and HQ State
if (has_gdp) {
  org_data <- org_data |>
    left_join(
      gdp_data,
      by = c("YEAR", "F9_00_ORG_ADDR_STATE" = "state_abb")
    )
}

# Remove temporary/redundant columns
org_data <- org_data |>
  select(
    -Year,                # Redundant (we keep 'YEAR')
    -F9_00_ORG_ADDR_CNTR  # Country code is constant
  )

if (has_gdp && "state" %in% names(org_data)) {
  org_data <- org_data |> select(-state)
}

# Join 2: Attach industry classifications (NTEE categories/foundation status) by EIN
if (has_activity) {
  org_data <- org_data |>
    left_join(
      activity_data,
      by = c("ORG_EIN" = "EIN")
    )
}

# ==============================================================================
# 5. Export Master Dataset
# ==============================================================================
saveRDS(org_data, "../Final_Data/clean_master_data.rds")

# Save as .csv for easy external inspection/sharing
write_csv(org_data, "../Final_Data/clean_master_data.csv")
