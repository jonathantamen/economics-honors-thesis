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
  "total_program_expenses",
  "total_expenses",
  # Supply side variables
  "donation_revenue",
  "membership_revenue",
  "fundraising_revenue",
  "government_revenue",
  "investment_revenue",
  "other_revenue",
  "total_revenue"
)

#--------------------------
# Part 2: Filtering Data Set
# Helper function to track rows
track_rows <- function(data, step_name) {
  cat(paste0(step_name, ": ", nrow(data), " rows remaining\n"))
  data
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
    total_expenses = "F9_09_EXP_TOT_TOT",
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
  filter(if_all(all_of(negatives_to_remove), ~ .x >= 0)) |>
  track_rows("After Step 4 (Dropping negative values)") |>
  #---------------------
  # Step 5: Filtering to only 50 US States
  # Here, I will remove any organizations that are not in the 50 US States.
  filter(state %in% state.abb) |>
  track_rows("After Step 5 (Dropping international orgs.)") |>
  # filters to organizations that have 'state' = a US state abbreviation
  #---------------------
  # Step 6: Restricting data set to specific variables
  select(
    organization_ein,
    organization_name,
    state,
    year,
    industry,
    total_program_expenses,
    total_expenses,
    total_revenue,
    donation_revenue,
    fundraising_revenue,
    government_revenue,
    membership_revenue,
    investment_revenue,
    other_revenue,
    gdp_change_percent
  ) |>
  #---------------------
  # Step 7: remove singleton fixed-effects
  group_by(organization_ein) |>
  filter(n() > 1) |>
  ungroup() |>
  group_by(state) |>
  filter(n() > 1) |>
  ungroup() |>
  group_by(industry) |>
  filter(n() > 1) |>
  ungroup() |>
  group_by(year) |>
  filter(n() > 1) |>
  ungroup() |>
  track_rows("After Step 7 (Dropping singleton fixed-effects)") |>
  #---------------------
  # Step 8: Creating log variables
  mutate(
    across(
      c(
        total_program_expenses,
        total_expenses,
        total_revenue,
        donation_revenue,
        fundraising_revenue,
        government_revenue,
        membership_revenue,
        investment_revenue,
        other_revenue
      ),
      ~ log(.x + 1),
      .names = "log_{.col}"
    )
  )
# SAVING
saveRDS(final_org_data, "../Final_Data/final_data_set.rds")
