# Imports
library(dplyr)
library(tidyr)
library(flextable) # For styling tables in Word
library(officer) # For exporting to Word documents

# Load data
final_org_data <- readRDS("../Final_Data/final_data_set.rds")

# 1. Variable Descriptions
# Creating a data frame to document our variables
variable_descriptions <- data.frame(
    Variable = c(
        "organization_ein",
        "organization_name",
        "state",
        "year",
        "industry",
        "total_program_expenses",
        "total_revenue",
        "donation_revenue",
        "fundraising_revenue",
        "government_revenue",
        "membership_revenue",
        "investment_revenue",
        "other_revenue"
    ),
    Description = c(
        "Unique identifier (Employer Identification Number)",
        "Name of the non-profit organization",
        "State where the organization is located",
        "Tax year of the filing and GDP record",
        "Primary activity/industry category",
        "Total expenses directed towards programs. Represents the demand for the organization's services.",
        "Total revenue from all sources. Represents the supply of the organization's services.",
        "Revenue from campaigns and direct donations",
        "Revenue from fundraising events",
        "Revenue from government grants",
        "Revenue from membership dues",
        "Net income from investments",
        "Miscellaneous revenue from other sources (i.e. sale of property, etc.)"
    )
)

# 2. Key Variables Setup
# Separate numeric from categorical (character) variables
numeric_vars <- c(
    "total_program_expenses",
    "total_revenue",
    "donation_revenue",
    "fundraising_revenue",
    "government_revenue",
    "membership_revenue",
    "investment_revenue",
    "other_revenue",
)

categorical_vars <- c(
    "organization_ein",
    "organization_name",
    "state",
    "industry",
    "year"
)

# 3. Generating Descriptive Statistics

# Calculate mathematical statistics for numeric variables
numeric_summary <- final_org_data |>
    select(all_of(numeric_vars)) |>
    # Pivot stretches the columns out so Variable names are rows, and values line up
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") |>
    group_by(Variable) |>
    summarize(
        Observations = sum(!is.na(Value)),
        Mean = mean(Value, na.rm = TRUE),
        Std_Dev = sd(Value, na.rm = TRUE),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE)
    )

# Calculate observations for text/categorical variables (Mean, Min, Max do not apply)
categorical_summary <- final_org_data |>
    select(all_of(categorical_vars)) |>
    # Ensure all values are purely characters first
    mutate(across(everything(), as.character)) |>
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") |>
    group_by(Variable) |>
    summarize(
        Observations = n_distinct(Value, na.rm = TRUE),
        # Use NA_real_ to force these into an empty numeric, matching the math table above
        Mean = NA_real_,
        Std_Dev = NA_real_,
        Min = NA_real_,
        Max = NA_real_
    )

# 4. Final Table Assembly
# Stack our two tables using bind_rows, and attach the plain English descriptions
final_summary_table <- bind_rows(numeric_summary, categorical_summary) |>
    left_join(variable_descriptions, by = "Variable") |>
    # Reorganize column order for readability
    select(Variable, Description, Observations, Mean, Std_Dev, Min, Max)

# Display the polished data frame
print(final_summary_table)

# 5. Exporting to Word Document
# Set up an empty Word document
doc <- read_docx()

# Convert the data frame into a formatted flextable
flex_table <- flextable(final_summary_table) |>
    theme_vanilla() |>
    autofit() |>
    font(fontname = "Times New Roman", part = "all") |>
    set_caption("Descriptive Statistics for Selected Variables") |>
    add_footer_lines("* Observations for categorical variables represent unique counts.")

# Add the table to the Word document
doc <- body_add_flextable(doc, value = flex_table)

# Save the document to your final data folder
print(doc, target = "../Outputs/Exploratory_Data_Analysis_summary_statistics.docx")
