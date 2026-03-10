## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE---------------------------------------------
# Load libraries
library(tidyverse)
library(fixest)
library(modelsummary)
library(flextable)

# Load data
# Pro-tip: use getwd() to verify your working directory
data_set <- readRDS("../Final_Data/final_data_set.rds")


## -----------------------------------------------------------------------------
# 1. Identify organizations that expanded during Covid (2019 vs 2020)
org_expansion <- data_set |>
  filter(year %in% c(2019, 2020)) |>
  select(organization_ein, year, total_revenue) |>
  # Handle duplicates if any (summing revenue for the year just in case)
  group_by(organization_ein, year) |>
  summarise(
    total_revenue = sum(total_revenue, na.rm = TRUE), .groups = "drop"
  ) |>
  pivot_wider(
    names_from = year, values_from = total_revenue, names_prefix = "rev_"
  ) |>
  # Create the expansion dummy: 1 if 2020 revenue > 2019 revenue
  mutate(expanded_during_covid = ifelse(rev_2020 > rev_2019, 1, 0)) |>
  select(organization_ein, expanded_during_covid) |>
  # Remove NAs (orgs that didn't exist in both years)
  filter(!is.na(expanded_during_covid))

# 2. Merge back and create variables for regression
regression_data <- data_set |>
  # Join the expansion info (left join keeps all orgs, but non-matching ones get NA for expansion)
  left_join(org_expansion, by = "organization_ein") |>
  mutate(
    # Post-Covid Dummy (Year >= 2020)
    post_covid = ifelse(year >= 2020, 1, 0)
  )

# Check how many organizations we successfully labeled
cat("Number of organizations with expansion data:", sum(!is.na(regression_data$expanded_during_covid)), "\n")


## -----------------------------------------------------------------------------
model_expansion_predictors <- feols(
  expanded_during_covid ~ log_donation_revenue + log_fundraising_revenue + log_government_revenue +
    log_membership_revenue + log_investment_revenue + log_other_revenue | year + state + industry,
  data = regression_data
)

summary(model_expansion_predictors)

## -----------------------------------------------------------------------------
models_list <- list(
  "Predicting Expansion" = model_expansion_predictors
)

interaction_table <- modelsummary(
  models_list,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "FE: year", "FE: state", "FE: industry"),
  coef_rename = c(
    "log_donation_revenue" = "Log(Donations)",
    "log_fundraising_revenue" = "Log(Fundraising)",
    "log_government_revenue" = "Log(Govt Grants)",
    "log_membership_revenue" = "Log(Membership)",
    "log_investment_revenue" = "Log(Investment)",
    "log_other_revenue" = "Log(Other)"
  ),
  title = "Regression Results: Predicting Expansion During Covid",
  output = "flextable"
) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

# Save to Word
save_as_docx(interaction_table, path = "../Outputs/7-covid_interaction_regressions.docx")
cat("Table saved to ../Outputs/7-covid_interaction_regressions.docx\n")

## -----------------------------------------------------------------------------
print(interaction_table)
