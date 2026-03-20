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
  select(organization_ein, year, log_total_program_expenses) |>
  pivot_wider(
    names_from = year, values_from = log_total_program_expenses, names_prefix = "log_exp_"
  ) |>
  # Create the expansion dummy: 1 if 2020 program expenses > 2019 program expenses
  mutate(expanded_during_covid = ifelse(log_exp_2020 > log_exp_2019, 1, 0)) |>
  select(organization_ein, expanded_during_covid) |>
  # Remove NAs (orgs that didn't exist in both years)
  filter(!is.na(expanded_during_covid))

# 1.1 Robustness Check: Organizations that expanded pre-Covid (2018 vs 2019)
org_expansion_robustness <- data_set |>
  filter(year %in% c(2018, 2019)) |>
  select(organization_ein, year, log_total_program_expenses) |>
  pivot_wider(
    names_from = year, values_from = log_total_program_expenses, names_prefix = "log_exp_"
  ) |>
  # Create the expansion dummy: 1 if 2019 program expenses > 2018 program expenses
  mutate(expanded_pre_covid = ifelse(log_exp_2019 > log_exp_2018, 1, 0)) |>
  select(organization_ein, expanded_pre_covid) |>
  filter(!is.na(expanded_pre_covid))

# 2. Merge back and create variables for regression
regression_data <- data_set |>
  # Join the expansion info (left join keeps all orgs, but non-matching ones get NA for expansion)
  left_join(org_expansion, by = "organization_ein") |>
  left_join(org_expansion_robustness, by = "organization_ein") |>
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

model_expansion_predictors_robustness <- feols(
  expanded_pre_covid ~ log_donation_revenue + log_fundraising_revenue + log_government_revenue +
    log_membership_revenue + log_investment_revenue + log_other_revenue | year + state + industry,
  data = regression_data
)

summary(model_expansion_predictors)
summary(model_expansion_predictors_robustness)

## -----------------------------------------------------------------------------
models_list <- list(
  "Covid Expansion" = model_expansion_predictors,
  "Pre-Covid Robustness" = model_expansion_predictors_robustness
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
  title = "Regression Results: Expansion During Covid vs Pre-Covid",
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
demand_interaction_model <- feols(
  log_total_program_expenses ~ gdp_change_percent * post_covid + log_total_revenue | year + state + industry + organization_ein,
  data = regression_data
)

supply_interaction_model <- feols(
  log_total_revenue ~ gdp_change_percent * post_covid | year + state + industry + organization_ein,
  data = regression_data
)

post_covid_models <- list(
  "Demand (Post-Covid Interactions)" = demand_interaction_model,
  "Supply (Post-Covid Interactions)" = supply_interaction_model
)

post_covid_table <- modelsummary(
  post_covid_models,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
  coef_rename = c(
    "gdp_change_percent" = "GDP Change %",
    "post_covid" = "Post-Covid Dummy",
    "gdp_change_percent:post_covid" = "GDP Change % x Post-Covid",
    "log_total_revenue" = "Log(Revenue)"
  ),
  title = "Regression Results: Post-Covid Growth Interactions",
  output = "flextable"
) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

# Re-save to Word appending the second table
save_as_docx(interaction_table, post_covid_table, path = "../Outputs/7-covid_interaction_regressions.docx")
cat("Updated tables saved to ../Outputs/7-covid_interaction_regressions.docx\n")


## -----------------------------------------------------------------------------
print(interaction_table)
