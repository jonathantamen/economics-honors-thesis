# Imports
library(dplyr)
library(fixest)
library(modelsummary)
library(flextable)

# 1. Load Data
data <- readRDS("../Final_Data/final_data_set.rds")

# 2. Prepare Data
# Select relevant variables and create log transformations
regression_data <- data

# 3. Define Regression Variables
# List of dependent variables (Y) to run against gdp_change_percent (X)
dependent_vars <- c(
    "log_donation_revenue",
    "log_fundraising_revenue",
    "log_government_revenue",
    "log_membership_revenue",
    "log_investment_revenue",
    "log_other_revenue"
)

# Friendly names for the table
var_titles <- c(
    "log_donation_revenue" = "Donations",
    "log_fundraising_revenue" = "Fundraising",
    "log_government_revenue" = "Govt Grants",
    "log_membership_revenue" = "Membership",
    "log_investment_revenue" = "Investment",
    "log_other_revenue" = "Other"
)

# 4. Run Regressions (Loop)
# We want to see: Y ~ gdp_change_percent | year + state + industry
model_list <- list()

for (var in dependent_vars) {
    # Create formula: var ~ gdp_change_percent | Fixed Effects
    fml <- as.formula(
        paste(var, "~ gdp_change_percent | year + state + industry + organization_ein")
    )

    # Run regression
    model <- feols(fml, data = regression_data)

    # Store with a nice name
    model_name <- var_titles[var]
    model_list[[model_name]] <- model
}

# 5. Output Results

# Create the comparison table
comparison_table <- modelsummary(
    model_list,
    stars = TRUE,
    gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
    coef_rename = c("gdp_change_percent" = "GDP Change %"),
    title = "Cyclicality of Non-Profit Revenue Sources (Industry & State FE)",
    output = "flextable" # Can change to "markdown" or "dataframe" to view in R
) |>
    autofit() |>
    theme_vanilla() |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header")

# Print to console/viewer
print(comparison_table)

# Export to Word (Optional)
save_as_docx(
    comparison_table,
    path = "../Outputs/3-explanatory_variables_experiment.docx"
)

# 6. Organization Fixed Effects Experiment
model_list_org_fe <- list()

for (var in dependent_vars) {
    # Create formula: var ~ gdp_change_percent | Fixed Effects + Organization
    fml_str <- paste(
        var, "~ gdp_change_percent | year + state + industry + organization_ein"
    )
    fml <- as.formula(fml_str)

    # Run regression
    model <- feols(fml, data = regression_data)

    # Store with a nice name
    model_name <- var_titles[var]
    model_list_org_fe[[model_name]] <- model
}

# 7. Output Results (Organization FE)

comparison_table_org_fe <- modelsummary(
    model_list_org_fe,
    stars = TRUE,
    gof_map = c(
        "nobs", "r.squared", "FE: year", "FE: state", "FE: industry",
        "FE: organization_ein"
    ),
    coef_rename = c("gdp_change_percent" = "GDP Change %"),
    title = "Cyclicality of Non-Profit Revenue Sources (Organization FE)",
    output = "flextable"
) |>
    autofit() |>
    theme_vanilla() |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header")

print(comparison_table_org_fe)
save_as_docx(
    comparison_table_org_fe,
    path = "../Outputs/4-explanatory_variables_experiment_org_fe.docx"
)
