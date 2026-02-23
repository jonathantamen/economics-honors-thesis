# Imports
# Packages
library(dplyr)
library(tidyverse)

# Data frame
org_data <- readRDS("../Final_Data/clean_master_data.rds")


# Description of what I'm doing
# This code is for narrowing my data set. An extension to my data cleaning.

#--------------------------
# Part 1: Set-up

# Setup function to find zero-activity organizations
bad_org_ids <- org_data |> # Thanks, AI.
  filter(YEAR < 2020) |>
  arrange(ORG_EIN, YEAR) |>
  group_by(ORG_EIN) |>
  summarize(max_streak = {
    both_zero <- (F9_08_REV_TOT_TOT == 0 & F9_09_EXP_TOT_PROG == 0)
    # organizations with zero expenses and zero revenues
    runs <- rle(both_zero)
    if (any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) |>
  filter(max_streak >= 3) |>
  pull(ORG_EIN) # got my list of organizations inactive for 3 or more years

# Key variables: organization_ein, organization_name, state, year, industry, total_program_expenses, total_revenue, donation_revenue, fundraising_revenue, government_revenue, membership_revenue, investment_revenue, other_revenue # nolint: line_length_linter.

# Setup for removing negative financial values
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

#--------------------------
# Part 2: Filtering Data Set
# Helper function to track rows
track_rows <- function(data, step_name) {
  cat(paste0(step_name, ": ", nrow(data), " rows remaining\n"))
  return(data)
}

# Main filtering function
final_org_data <- org_data |>
  track_rows("Initial Data") |>
  #----------------------
  # Step 1: Renaming columns
  rename(
    # Identifier variables
    organization_ein = "ORG_EIN",
    organization_name = "F9_00_ORG_NAME_L1",
    state = "F9_00_ORG_ADDR_STATE",
    year = "YEAR",
    industry = "activity_category",
    # Key variables
    total_program_expenses = "F9_09_EXP_TOT_PROG",
    total_revenue = "F9_08_REV_TOT_TOT",

    # Explanatory variables
    donation_revenue = "F9_08_REV_CONTR_FED_CAMP",
    fundraising_revenue = "F9_08_REV_CONTR_FUNDR_EVNT",
    government_revenue = "F9_08_REV_CONTR_GOVT_GRANT",
    membership_revenue = "F9_08_REV_CONTR_MEMBSHIP_DUE",
    investment_revenue = "F9_08_REV_OTH_INV_NET_TOT",
    other_revenue = "F9_08_REV_MISC_TOT_TOT",
  ) |>
  track_rows("After Step 1 (Renaming columns)") |>
  #--------------------------------------------
  # Step 2: Dropping zero activity organizations
  filter(!(organization_ein %in% bad_org_ids)) |>
  # filters to organizations that are NOT organizations in bad_eins
  track_rows("After Step 2 (Dropping zero activity orgs)") |>
  #----------------------------------
  # Step 3: Dropping hospitals/schools
  filter(org_type == "Neither") |>
  track_rows("After Step 3 (Dropping hospitals/schools)") |>
  # filters to organizations that are neither hospitals nor schools
  #--------------------------
  # Step 4: Removing negative values
  # Here, I will remove any negative values in specific financial variables.
  # Negative revenues or expenses are impossible to compute with.
  filter(across(all_of(negatives_to_remove), ~ .x >= 0)) |>
  #---------------------
  # Step 5: Creating log variables
  # Here, I will create log variables for the financial variables.
  # This is to account for the skewness of the data.
  # This makes it easier to use these variables later in my regression analysis.
  mutate(
    log_total_program_expenses = log(total_program_expenses),
    log_total_revenue = log(total_revenue),
    log_donation_revenue = log(donation_revenue),
    log_fundraising_revenue = log(fundraising_revenue),
    log_government_revenue = log(government_revenue),
    log_membership_revenue = log(membership_revenue),
    log_investment_revenue = log(investment_revenue),
    log_other_revenue = log(other_revenue)
  ) |>
  #---------------------
  # Step 6: Filtering to only 50 US States
  # Here, I will remove any organizations that are not in the 50 US States.
  filter(state %in% state.abb) |>
  # filters to organizations that have 'state' = a US state abbreviation
  #---------------------
  # SAVING
  saveRDS(final_org_data, "../Final_Data/final_data_set.rds")
