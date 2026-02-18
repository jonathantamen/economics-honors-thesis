# Imports
library(dplyr)
library(fixest)
library(modelsummary)
library(flextable)

# 1. Load Data
data <- readRDS("../Final_Data/final_data_set.rds")

# 2. Prepare Data
# Select relevant variables and create log transformations
regression_data <- data %>%
    select(
        organization_ein, year, state, industry, gdp_change_percent,
        # Dependent Variables to test
        donation_revenue,
        fundraising_revenue,
        government_revenue,
        membership_revenue,
        investment_revenue,
        other_revenue,
        total_revenue,
        total_program_expenses
    ) %>%
    mutate(
        # Create Log transformations (log(x + 1) to handle zeros)
        log_donations = log(donation_revenue + 1),
        log_fundraising = log(fundraising_revenue + 1),
        log_government = log(government_revenue + 1),
        log_membership = log(membership_revenue + 1),
        log_investment = log(investment_revenue + 1),
        log_other = log(other_revenue + 1),
        log_total_revenue = log(total_revenue + 1),
        log_program_expenses = log(total_program_expenses + 1)
    )

# 3. Define Regression Variables
# List of dependent variables (Y) to run against gdp_change_percent (X)
dependent_vars <- c(
    "log_donations",
    "log_fundraising",
    "log_government",
    "log_membership",
    "log_investment",
    "log_other"
)

# Friendly names for the table
var_titles <- c(
    "log_donations" = "Donations",
    "log_fundraising" = "Fundraising",
    "log_government" = "Govt Grants",
    "log_membership" = "Membership",
    "log_investment" = "Investment",
    "log_other" = "Other"
)

# 4. Run Regressions (Loop)
# We want to see: Y ~ gdp_change_percent | year + state + industry
model_list <- list()

for (var in dependent_vars) {
    # Create formula: var ~ gdp_change_percent | Fixed Effects
    fml <- as.formula(paste(var, "~ gdp_change_percent | year + state + industry"))

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
    gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry"),
    coef_rename = c("gdp_change_percent" = "GDP Change %"),
    title = "Cyclicality of Non-Profit Revenue Sources (Industry & State FE)",
    output = "flextable", # Can change to "markdown" or "dataframe" to view in RStudio
    font = "Times New Roman"
) %>%
    autofit() %>%
    theme_vanilla() %>%
    bold(part = "header")

# Print to console/viewer
print(comparison_table)

# Export to Word (Optional)
save_as_docx(comparison_table, path = "../Outputs/explanatory_variables_experiment.docx")
