# ==============================================================================
# 1. Set-up
# This script will be for basic data clean up of my raw data.
# ==============================================================================

# Imports
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
setwd("/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Scripts")

# Import organization data
# aka, my main data set
org_data <- readRDS(
  "../Final_Data/final_master_data.rds"
)

# Import gdp data
gdp_data <- readRDS("../Final_Data/gdp_data.rds")

gdp_data <- gdp_data |>
  mutate(YEAR = as.integer(YEAR)) # Making sure data types match
org_data <- org_data |>
  mutate(YEAR = as.integer(Year)) # Making sure data types match

# Import activity type data
activity_data <- readRDS("../Final_Data/activity_data.rds")

# ==============================================================================
# 2. CLEANING ORGANIZATION DATA
# ==============================================================================
# Here, I will replace any empty cell with a 0 for the financial variables.
# This makes calculation easier to do.
columns_to_replace <- c(
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
  "F9_09_EXP_GRANT_US_ORG_TOT",
  "F9_09_EXP_GRANT_US_INDIV_TOT",
  "F9_09_EXP_GRANT_FRGN_TOT",
  "F9_09_EXP_TOT_TOT",
  "F9_09_EXP_TOT_PROG",
  "F9_09_EXP_TOT_MGMT",
  "F9_09_EXP_TOT_FUNDR"
)
org_data <- org_data |> # This function replaces all of the nulls in the above columns with a "0"
  mutate(across(
    .cols = all_of(columns_to_replace),
    .fns = ~ replace_na(.x, 0)
  ))


# Here, I will remove any negative values in specific financial variables.
# Negative revenues or expenses are impossible to compute with.
# Also, they are rare in the data set (occuring ~500 times)

negatives_to_remove <- c(
  # Demand side variables
  "F9_09_EXP_GRANT_US_ORG_TOT",
  "F9_09_EXP_GRANT_US_INDIV_TOT",
  "F9_09_EXP_GRANT_FRGN_TOT",
  "F9_09_EXP_TOT_TOT",
  "F9_09_EXP_TOT_PROG",
  "F9_09_EXP_TOT_MGMT",
  "F9_09_EXP_TOT_FUNDR",

  # Supply side variables
  "F9_08_REV_CONTR_FED_CAMP",
  "F9_08_REV_PROG_TOT_TOT",
  "F9_08_REV_CONTR_MEMBSHIP_DUE",
  "F9_08_REV_CONTR_FUNDR_EVNT",
  "F9_08_REV_CONTR_RLTD_ORG",
  "F9_08_REV_CONTR_GOVT_GRANT",
  "F9_08_REV_CONTR_OTH",
  "F9_08_REV_CONTR_TOT",
  "F9_08_REV_OTH_INV_NET_TOT",
  "F9_08_REV_MISC_TOT_TOT",
  "F9_08_REV_TOT_TOT",
  "F9_08_REV_TOT_TOT"
)
org_data <- org_data |>
  filter(if_all(
    .cols = all_of(negatives_to_remove),
    .fns = ~ .x >= 0
  ))

# Here, I will eliminate any organization whose HQ is not in the United States.
# International organizations will make my data harder to compute.
# International organizations will also have other unobserved variables.
# I want my data to be only state abbreviations.

# Formatting my data for all USA states to a format usable for the filter function.
# Filtering my data to include only USA organizations. Removes ~2,500
org_data <- org_data |>
  filter(F9_00_ORG_ADDR_STATE %in% state.abb)


# ==============================================================================
# 3. CLEANING GDP DATA
# ==============================================================================
# I need to change the full name to abbreviations.
state_lookup <- data.frame(
  state_name = state.name,
  state_abb = state.abb,
  stringsAsFactors = FALSE
)

gdp_data <- gdp_data |>
  left_join(
    state_lookup,
    by = c("state" = "state_name")
  )

# ==============================================================================
# 4. GROUPING AND SORTING
# ==============================================================================
org_data <- org_data |>
  arrange(ORG_EIN, Year)

# ==============================================================================
# 5. COMBINING GDP DATA TO ORG DATA
# ==============================================================================

# Joining the data now.
org_data <- org_data |>
  left_join(
    gdp_data,
    by = c("YEAR",
      "F9_00_ORG_ADDR_STATE" = "state_abb"
    )
  )

# Now I will drop redundant columns.
org_data <- org_data |>
  select(
    -Year,
    -state,
    -F9_00_ORG_ADDR_CNTR,
  )

# ==============================================================================
# 6. COMBINING ACTIVITY TYPE DATA TO ORG DATA
# ==============================================================================
# This section is where I will combine my data for each organization (by EIN)
# Adding activity/NTEE codes that classify the type of organization
# This becomes new fixed-effect variables for my analysis
# Examples: A##: Arts, Culture and Humanities, B##: Education

org_data <- org_data |>
  left_join(
    activity_data,
    by = c("ORG_EIN" = "EIN")
  )

# ==============================================================================
# FINAL. SAVING
# ==============================================================================
saveRDS(org_data, "../Final_Data/clean_master_data.rds")

# Save as .csv for easy sharing
write_csv(org_data, "../Final_Data/clean_master_data.csv")
