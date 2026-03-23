## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
data <- readRDS("../Final_Data/final_data_set.rds")

# Packages
library(dplyr)
library(fixest)
library(modelsummary) # Best package for regression tables
library(flextable) # For Word tables
## -----------------------------------------------------------------------------
regression_data <- data |>
  select(total_program_expenses, total_revenue, gdp_change_percent, year, state, industry, organization_ein, log_total_program_expenses, log_total_revenue)

## -----1. Simple correlation-----
model1demand <- feols(log_total_program_expenses ~ gdp_change_percent, data = regression_data)
model1supply <- feols(log_total_revenue ~ gdp_change_percent, data = regression_data)
etable(model1demand, model1supply)
## -----2. Adding yearly data-----
model2demand <- feols(log_total_program_expenses ~ gdp_change_percent | year, data = regression_data)
model2supply <- feols(log_total_revenue ~ gdp_change_percent | year, data = regression_data)
etable(model2demand, model2supply)
## -----3. Adding state data-----
macro_fixed_effects_demand_model <- feols(log_total_program_expenses ~ gdp_change_percent | year + state, data = regression_data)
macro_fixed_effects_supply_model <- feols(log_total_revenue ~ gdp_change_percent | year + state, data = regression_data)
# This creates my macro_fixed_effects_model
etable(macro_fixed_effects_demand_model, macro_fixed_effects_supply_model)
## -----4. Add  ing industry data fixed effects-----
industry_fixed_effects_demand_model <- feols(log_total_program_expenses ~ gdp_change_percent | year + state + industry, data = regression_data)
industry_fixed_effects_supply_model <- feols(log_total_revenue ~ gdp_change_percent | year + state + industry, data = regression_data)
# This creates my industry_fixed_effects_model
etable(industry_fixed_effects_demand_model, industry_fixed_effects_supply_model)
## -----5. Adding firm data fixed effects-----
# 5. Adding firm data fixed effects
firm_fixed_effects_demand_model <- feols(log_total_program_expenses ~ gdp_change_percent | year + state + industry + organization_ein, data = regression_data)
firm_fixed_effects_supply_model <- feols(log_total_revenue ~ gdp_change_percent | year + state + industry + organization_ein, data = regression_data)
# This creates my firm_fixed_effects_model
etable(firm_fixed_effects_demand_model, firm_fixed_effects_supply_model)
# This model is inline with the literature
## -----6. Adding revenue control-----
revenue_controlled_demand_model <- feols(log_total_program_expenses ~ gdp_change_percent + log_total_revenue | year + state + industry + organization_ein, data = regression_data)
# This creates my revenue_controlled_model
summary(revenue_controlled_demand_model)

## -----Comparison Table and Output-----
model_list <- list(
  "Demand Side (Firm)" = firm_fixed_effects_demand_model,
  "Supply Side (Firm)" = firm_fixed_effects_supply_model,
  "Revenue Controlled" = revenue_controlled_demand_model
)

comparison_table <- modelsummary(
  model_list,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
  coef_rename = c(
    "gdp_change_percent" = "GDP Change %",
    "log_total_revenue" = "Log(Revenue)"
  ),
  title = "Non-Profit Economic Cycle: Demand-side vs. Supply-side",
  output = "flextable"
) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

save_as_docx(comparison_table, path = "../Outputs/2-preliminary_regression_table.docx")

## -----------------------------------------------------------------------------
print(comparison_table)
## -----Iterative Regressions Comparison Table and Output-----
iterative_demand_models <- list(
  "Demand (Base)" = model1demand,
  "Demand (+Year)" = model2demand,
  "Demand (+State)" = macro_fixed_effects_demand_model,
  "Demand (+Industry)" = industry_fixed_effects_demand_model,
  "Demand (+Firm)" = firm_fixed_effects_demand_model
)

iterative_supply_models <- list(
  "Supply (Base)" = model1supply,
  "Supply (+Year)" = model2supply,
  "Supply (+State)" = macro_fixed_effects_supply_model,
  "Supply (+Industry)" = industry_fixed_effects_supply_model,
  "Supply (+Firm)" = firm_fixed_effects_supply_model
)

iterative_demand_table <- modelsummary(
  iterative_demand_models,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
  coef_rename = c("gdp_change_percent" = "GDP Change %"),
  title = "Iterative Demand Side Results",
  output = "flextable"
) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

iterative_supply_table <- modelsummary(
  iterative_supply_models,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
  coef_rename = c("gdp_change_percent" = "GDP Change %"),
  title = "Iterative Supply Side Results",
  output = "flextable"
) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

save_as_docx(iterative_demand_table, iterative_supply_table, path = "../Outputs/6-iterative_regression_tables.docx")
## -----------------------------------------------------------------------------
print(iterative_demand_table)
print(iterative_supply_table)
