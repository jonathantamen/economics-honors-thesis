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
    # Log transformations
    log_program_expenses = log(total_program_expenses + 1),
    log_revenue = log(total_revenue + 1),

    # Post-Covid Dummy (Year >= 2020)
    post_covid = ifelse(year >= 2020, 1, 0)
  )

# Check how many organizations we successfully labeled
cat("Number of organizations with expansion data:", sum(!is.na(regression_data$expanded_during_covid)), "\n")


## -----------------------------------------------------------------------------
model_covid_interaction <- feols(
  log_program_expenses ~ gdp_change_percent * post_covid + log_revenue | year + state + industry + organization_ein,
  data = regression_data
)

# Revenue Model
model_covid_interaction_rev <- feols(
  log_revenue ~ gdp_change_percent * post_covid | year + state + industry + organization_ein,
  data = regression_data
)

summary(model_covid_interaction)
summary(model_covid_interaction_rev)


## -----------------------------------------------------------------------------
model_expansion_interaction <- feols(
  log_program_expenses ~ gdp_change_percent * expanded_during_covid + log_revenue | year + state + industry + organization_ein,
  data = regression_data
)

# Revenue Model
model_expansion_interaction_rev <- feols(
  log_revenue ~ gdp_change_percent * expanded_during_covid | year + state + industry + organization_ein,
  data = regression_data
)

summary(model_expansion_interaction)
summary(model_expansion_interaction_rev)


## -----------------------------------------------------------------------------
model_combined <- feols(
  log_program_expenses ~ gdp_change_percent * post_covid +
    gdp_change_percent * expanded_during_covid +
    log_revenue | year + state + industry + organization_ein,
  data = regression_data
)

# Revenue Model
model_combined_rev <- feols(
  log_revenue ~ gdp_change_percent * post_covid +
    gdp_change_percent * expanded_during_covid | year + state + industry + organization_ein,
  data = regression_data
)

summary(model_combined)
summary(model_combined_rev)


## -----------------------------------------------------------------------------
models_list <- list(
  "Covid Interaction (Expenses)" = model_covid_interaction,
  "Covid Interaction (Revenue)" = model_covid_interaction_rev,
  "Expansion Interaction (Expenses)" = model_expansion_interaction,
  "Expansion Interaction (Revenue)" = model_expansion_interaction_rev,
  "Combined (Expenses)" = model_combined,
  "Combined (Revenue)" = model_combined_rev
)

interaction_table <- modelsummary(
  models_list,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
  coef_rename = c(
    "gdp_change_percent" = "GDP Change %",
    "log_revenue" = "Log(Revenue)",
    "post_covid" = "Post-Covid",
    "gdp_change_percent:post_covid" = "GDP % × Post-Covid",
    "expanded_during_covid" = "Expanded Org",
    "gdp_change_percent:expanded_during_covid" = "GDP % × Expanded Org"
  ),
  title = "Regression Results: Covid & Expansion Interactions",
  output = "flextable"
) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

# Save to Word
save_as_docx(interaction_table, path = "../Outputs/7-covid_interaction_regressions.docx")
cat("Table saved to ../Outputs/covid_interaction_regressions.docx\n")


## -----------------------------------------------------------------------------
print(interaction_table)
