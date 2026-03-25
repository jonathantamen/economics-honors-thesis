# Imports
library(dplyr)
library(fixest)
library(modelsummary)
library(flextable)

# 1. Load Data
data <- readRDS("../Final_Data/final_data_set.rds")

# 2. Define Explanatory Variables
explanatory_vars <- c(
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

# 3. Set Up Interaction Models
interaction_models <- list()

# 4. Run Regressions (Loop)
# We want to interact GDP Change % with each explanatory variable
# Dependent variable assumed to be log_total_program_expenses (Demand side)
for (var in explanatory_vars) {
    # Formula: log_total_program_expenses ~ gdp_change_percent * var | year + state + industry + organization_ein
    fml_str <- paste(
        "log_total_program_expenses ~ gdp_change_percent *", var,
        "| year + state + industry + organization_ein"
    )
    fml <- as.formula(fml_str)

    # Run regression
    model <- feols(fml, data = data)

    # Store with a nice name
    model_name <- var_titles[var]
    interaction_models[[model_name]] <- model
}

# 5. Output Results
# Create the comparison table
interaction_table <- modelsummary(
    interaction_models,
    stars = TRUE,
    gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
    coef_rename = c(
        "gdp_change_percent" = "GDP Change %",
        "log_donation_revenue" = "Log(Donations)",
        "log_fundraising_revenue" = "Log(Fundraising)",
        "log_government_revenue" = "Log(Govt Grants)",
        "log_membership_revenue" = "Log(Membership)",
        "log_investment_revenue" = "Log(Investment)",
        "log_other_revenue" = "Log(Other)",
        "gdp_change_percent:log_donation_revenue" = "GDP Change % x Log(Donations)",
        "gdp_change_percent:log_fundraising_revenue" = "GDP Change % x Log(Fundraising)",
        "gdp_change_percent:log_government_revenue" = "GDP Change % x Log(Govt Grants)",
        "gdp_change_percent:log_membership_revenue" = "GDP Change % x Log(Membership)",
        "gdp_change_percent:log_investment_revenue" = "GDP Change % x Log(Investment)",
        "gdp_change_percent:log_other_revenue" = "GDP Change % x Log(Other)"
    ),
    title = "Interaction Effects: Explanatory Variables on GDP Change %",
    output = "flextable"
) |>
    autofit() |>
    theme_vanilla() |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header")

# Print to console/viewer
print(interaction_table)

# Export to Word (Optional but useful)
save_as_docx(
    interaction_table,
    path = "../Outputs/4b-explanatory_variables_interactions.docx"
)
#------------------------------------------------------------------------------
# 6. Combined Interaction Regression
# Run a single regression explicitly writing out all variables for readability
combined_model <- feols(
    log_total_program_expenses ~ gdp_change_percent * (
        log_donation_revenue +
            log_fundraising_revenue +
            log_government_revenue +
            log_membership_revenue +
            log_investment_revenue +
            log_other_revenue
    ) | year + state + industry + organization_ein,
    data = data
)

# Create a separate output table for the combined model
combined_table <- modelsummary(
    list("Combined Interaction" = combined_model),
    stars = TRUE,
    gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry", "FE: organization_ein"),
    coef_rename = c(
        "gdp_change_percent" = "GDP Change %",
        "log_donation_revenue" = "Log(Donations)",
        "log_fundraising_revenue" = "Log(Fundraising)",
        "log_government_revenue" = "Log(Govt Grants)",
        "log_membership_revenue" = "Log(Membership)",
        "log_investment_revenue" = "Log(Investment)",
        "log_other_revenue" = "Log(Other)",
        "gdp_change_percent:log_donation_revenue" = "GDP Change % x Log(Donations)",
        "gdp_change_percent:log_fundraising_revenue" = "GDP Change % x Log(Fundraising)",
        "gdp_change_percent:log_government_revenue" = "GDP Change % x Log(Govt Grants)",
        "gdp_change_percent:log_membership_revenue" = "GDP Change % x Log(Membership)",
        "gdp_change_percent:log_investment_revenue" = "GDP Change % x Log(Investment)",
        "gdp_change_percent:log_other_revenue" = "GDP Change % x Log(Other)"
    ),
    title = "Combined Interaction Effects: All Variables on GDP Change %",
    output = "flextable"
) |>
    autofit() |>
    theme_vanilla() |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header")

print(combined_table)

# Export our independent combined document
save_as_docx(
    combined_table,
    path = "../Outputs/4b2-combined_interactions.docx"
)

#--7. Interaction Models Without Firm Fixed Effects--
interaction_models_no_fe <- list()

for (var in explanatory_vars) {
    # Formula without organization_ein
    fml_str <- paste(
        "log_total_program_expenses ~ gdp_change_percent *", var,
        "| year + state + industry"
    )
    fml <- as.formula(fml_str)

    model <- feols(fml, data = data)

    model_name <- var_titles[var]
    interaction_models_no_fe[[model_name]] <- model
}

interaction_table_no_fe <- modelsummary(
    interaction_models_no_fe,
    stars = TRUE,
    gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry"),
    coef_rename = c(
        "gdp_change_percent" = "GDP Change %",
        "log_donation_revenue" = "Log(Donations)",
        "log_fundraising_revenue" = "Log(Fundraising)",
        "log_government_revenue" = "Log(Govt Grants)",
        "log_membership_revenue" = "Log(Membership)",
        "log_investment_revenue" = "Log(Investment)",
        "log_other_revenue" = "Log(Other)",
        "gdp_change_percent:log_donation_revenue" = "GDP Change % x Log(Donations)",
        "gdp_change_percent:log_fundraising_revenue" = "GDP Change % x Log(Fundraising)",
        "gdp_change_percent:log_government_revenue" = "GDP Change % x Log(Govt Grants)",
        "gdp_change_percent:log_membership_revenue" = "GDP Change % x Log(Membership)",
        "gdp_change_percent:log_investment_revenue" = "GDP Change % x Log(Investment)",
        "gdp_change_percent:log_other_revenue" = "GDP Change % x Log(Other)"
    ),
    title = "Interaction Effects: Explanatory Variables on GDP Change % (No Firm FE)",
    output = "flextable"
) |>
    autofit() |>
    theme_vanilla() |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header")

print(interaction_table_no_fe)

save_as_docx(
    interaction_table_no_fe,
    path = "../Outputs/4b-explanatory_variables_interactions_no_fe.docx"
)

# 8. Combined Interaction Regression Without Firm Fixed Effects
combined_model_no_fe <- feols(
    log_total_program_expenses ~ gdp_change_percent * (
        log_donation_revenue +
            log_fundraising_revenue +
            log_government_revenue +
            log_membership_revenue +
            log_investment_revenue +
            log_other_revenue
    ) | year + state + industry,
    data = data
)

combined_table_no_fe <- modelsummary(
    list("Combined Interaction" = combined_model_no_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared", "FE: year", "FE: state", "FE: industry"),
    coef_rename = c(
        "gdp_change_percent" = "GDP Change %",
        "log_donation_revenue" = "Log(Donations)",
        "log_fundraising_revenue" = "Log(Fundraising)",
        "log_government_revenue" = "Log(Govt Grants)",
        "log_membership_revenue" = "Log(Membership)",
        "log_investment_revenue" = "Log(Investment)",
        "log_other_revenue" = "Log(Other)",
        "gdp_change_percent:log_donation_revenue" = "GDP Change % x Log(Donations)",
        "gdp_change_percent:log_fundraising_revenue" = "GDP Change % x Log(Fundraising)",
        "gdp_change_percent:log_government_revenue" = "GDP Change % x Log(Govt Grants)",
        "gdp_change_percent:log_membership_revenue" = "GDP Change % x Log(Membership)",
        "gdp_change_percent:log_investment_revenue" = "GDP Change % x Log(Investment)",
        "gdp_change_percent:log_other_revenue" = "GDP Change % x Log(Other)"
    ),
    title = "Combined Interaction Effects: All Variables on GDP Change % (No Firm FE)",
    output = "flextable"
) |>
    autofit() |>
    theme_vanilla() |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header")

print(combined_table_no_fe)

save_as_docx(
    combined_table_no_fe,
    path = "../Outputs/4b2-combined_interactions_no_fe.docx"
)
