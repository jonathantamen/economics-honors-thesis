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
regression_data <- data %>%
  select(total_program_expenses, total_revenue, gdp_change_percent, year, state, industry, organization_ein) %>%
  mutate(
    log_program_expenses = log(total_program_expenses + 1),
    log_revenues = log(total_revenue + 1),
    log_revenue = log(total_revenue + 1)
  )


## -----------------------------------------------------------------------------
# Regression code
macro_fixed_effects_demand_model <- feols(log_program_expenses ~ gdp_change_percent | year + state, data = regression_data)

macro_fixed_effects_supply_model <- feols(log_revenues ~ gdp_change_percent | year + state, data = regression_data)

# Output code
summary(macro_fixed_effects_demand_model) # Demand side
summary(macro_fixed_effects_supply_model) # Supply side


## -----------------------------------------------------------------------------
# Regression code
industry_fixed_effects_demand_model <- feols(log_program_expenses ~ gdp_change_percent | year + state + industry, data = regression_data)

industry_fixed_effects_supply_model <- feols(log_revenues ~ gdp_change_percent | year + state + industry, data = regression_data)

# Output code
summary(industry_fixed_effects_demand_model) # Demand
summary(industry_fixed_effects_supply_model) # Supply


## -----------------------------------------------------------------------------
# Regression code
firm_fixed_effects_demand_model <- feols(log_program_expenses ~ gdp_change_percent | year + state + industry + organization_ein, data = regression_data)

firm_fixed_effects_supply_model <- feols(log_revenues ~ gdp_change_percent | year + state + industry + organization_ein, data = regression_data)

# Output code
summary(firm_fixed_effects_demand_model)
summary(firm_fixed_effects_supply_model)


## -----------------------------------------------------------------------------
# Regression code
revenue_controlled_demand_model <- feols(log_program_expenses ~ gdp_change_percent + log_revenue | year + state + industry, data = regression_data)

# Output code
summary(revenue_controlled_demand_model)


## -----------------------------------------------------------------------------
model_list <- list(
  "Demand Side (Macro)" = macro_fixed_effects_demand_model,
  "Supply Side (Macro)" = macro_fixed_effects_supply_model,
  "Demand Side (Industry)" = industry_fixed_effects_demand_model,
  "Supply Side (Industry)" = industry_fixed_effects_supply_model,
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
    "log_revenue" = "Log(Revenue)"
  ),
  title = "Non-Profit Economic Cycle: Demand-side vs. Supply-side",
  output = "flextable"
) %>%
  autofit() %>%
  theme_vanilla() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  bold(part = "header")

save_as_docx(comparison_table, path = "../Outputs/2-preliminary_regression_table.docx")

## -----------------------------------------------------------------------------
print(comparison_table)
