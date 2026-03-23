# Imports
library(dplyr)
library(tidyr)
library(ggplot2) # For data visualization
library(flextable) # For styling tables in Word
library(officer) # For exporting to Word documents

# Load data
final_org_data <- readRDS("../Final_Data/final_data_set.rds")

#-------SUMMARY STATISTICS TABLE--------
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
        "other_revenue",
        "gdp_change_percent"
    ),
    Description = c(
        "Unique identifier (Employer Identification Number)",
        "Name of the non-profit organization",
        "State where the organization is located",
        "Tax year of the filing and GDP record",
        "Primary activity/industry category",
        paste(
            "Total expenses directed towards programs.",
            "Represents the demand for the organization's services."
        ),
        paste(
            "Total revenue from all sources.",
            "Represents the supply of the organization's services."
        ),
        "Revenue from campaigns and direct donations",
        "Revenue from fundraising events",
        "Revenue from government grants",
        "Revenue from membership dues",
        "Net income from investments",
        paste(
            "Miscellaneous revenue from other sources",
            "(i.e. sale of property, etc.)"
        ),
        "Percentage change in real GDP, by state"
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
    "gdp_change_percent"
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
    # Pivot stretches columns so Variable names are rows, and values line up
    pivot_longer(
        cols = everything(), names_to = "Variable", values_to = "Value"
    ) |>
    group_by(Variable) |>
    summarize(
        Observations = sum(!is.na(Value)),
        Mean = mean(Value, na.rm = TRUE),
        Std_Dev = sd(Value, na.rm = TRUE),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE)
    )
# Calculate observations for text/categorical variables
categorical_summary <- final_org_data |>
    select(all_of(categorical_vars)) |>
    # Ensure all values are purely characters first
    mutate(across(everything(), as.character)) |>
    pivot_longer(
        cols = everything(), names_to = "Variable", values_to = "Value"
    ) |>
    group_by(Variable) |>
    summarize(
        Observations = n_distinct(Value, na.rm = TRUE),
        # Use NA_real_ to force these into an empty numeric
        Mean = NA_real_,
        Std_Dev = NA_real_,
        Min = NA_real_,
        Max = NA_real_
    )
# 4. Final Table Assembly
# Stack our two tables using bind_rows, and attach descriptions
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
    add_footer_lines(
        "* Observations for categorical variables represent unique counts."
    )
# Add the table to the Word document
doc <- body_add_flextable(doc, value = flex_table)
# Save the document to your final data folder
print(
    doc,
    target = "../Outputs/5-Exploratory_Data_Analysis_summary_statistics.docx"
)

#-------------------------- EXPLORATORY VISUALIZATIONS--------------------------
# Disable scientific notation globally for this session
options(scipen = 999)

# We'll use a formatter function to add commas to our axis labels, making large numbers readable
format_comma <- function(x) format(x, big.mark = ",", scientific = FALSE)

# Build a histogram for the distribution of revenues
revenue_histogram <- ggplot(final_org_data, aes(x = log_total_revenue)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "black") +
    scale_y_continuous(labels = format_comma) +
    theme_minimal() +
    labs(
        title = "Distribution of Total Revenues",
        x = "Total Revenue ($)",
        y = "Count of Organizations"
    )

# Build a histogram for the distribution of program expenses
expenses_histogram <- ggplot(final_org_data, aes(x = log_total_program_expenses)) +
    geom_histogram(bins = 50, fill = "forestgreen", color = "black") +
    scale_y_continuous(labels = format_comma) +
    theme_minimal() +
    labs(
        title = "Distribution of Program Expenses",
        x = "Total Program Expenses ($)",
        y = "Count of Organizations"
    )

# Build a bar chart for the distribution of observations by year
year_bar_chart <- ggplot(final_org_data, aes(x = factor(year))) +
    geom_bar(fill = "coral", color = "black") +
    scale_y_continuous(labels = format_comma) +
    theme_minimal() +
    labs(
        title = "Observations Across Years",
        x = "Year",
        y = "Count of Observations"
    )

# Optional: Print the plots to view them
print(revenue_histogram)
print(expenses_histogram)
print(year_bar_chart)

# Save the visualizations to the Outputs folder
ggsave(
    filename = "../Outputs/2-Distribution_of_Total_Revenues.png",
    plot = revenue_histogram,
    width = 8,
    height = 6,
    dpi = 300
)

ggsave(
    filename = "../Outputs/3-Distribution_of_Program_Expenses.png",
    plot = expenses_histogram,
    width = 8,
    height = 6,
    dpi = 300
)

ggsave(
    filename = "../Outputs/4-Observations_Across_Years.png",
    plot = year_bar_chart,
    width = 8,
    height = 6,
    dpi = 300
)

#--------------------------INDUSTRY DISTRIBUTION TABLE--------------------------
# Create a mapping of industry names to their corresponding letters and count observations
industry_distribution <- final_org_data |>
    group_by(industry) |>
    summarize(Observations = n(), .groups = "drop") |>
    mutate(
        Letter = case_when(
            industry == "Arts, Culture & Humanities" ~ "A",
            industry == "Education" ~ "B",
            industry == "Environment" ~ "C",
            industry == "Animal-Related" ~ "D",
            industry == "Health Care" ~ "E",
            industry == "Mental Health & Crisis Intervention" ~ "F",
            industry == "Voluntary Health Associations & Medical Disciplines" ~ "G",
            industry == "Medical Research" ~ "H",
            industry == "Crime & Legal-Related" ~ "I",
            industry == "Employment" ~ "J",
            industry == "Food, Agriculture & Nutrition" ~ "K",
            industry == "Housing & Shelter" ~ "L",
            industry == "Public Safety" ~ "M",
            industry == "Recreation & Sports" ~ "N",
            industry == "Youth Development" ~ "O",
            industry == "Human Services" ~ "P",
            industry == "International, Foreign Affairs" ~ "Q",
            industry == "Civil Rights, Social Action & Advocacy" ~ "R",
            industry == "Community Improvement & Capacity Building" ~ "S",
            industry == "Philanthropy, Voluntarism & Grantmaking" ~ "T",
            industry == "Science & Technology" ~ "U",
            industry == "Social Science" ~ "V",
            industry == "Public & Societal Benefit" ~ "W",
            industry == "Religion-Related" ~ "X",
            industry == "Mutual & Membership Benefit" ~ "Y",
            industry == "Unknown or Unclassified" ~ "Z",
            TRUE ~ "Z"
        )
    ) |>
    # Reorder columns to have Letter first, rename industry to Industry
    select(Letter, Industry = industry, Observations) |>
    # Sort alphabetically by Letter
    arrange(Letter)

print(industry_distribution)

# Export this table to a new Word document
doc_industry <- read_docx()

flex_table_industry <- flextable(industry_distribution) |>
    theme_vanilla() |>
    autofit() |>
    font(fontname = "Times New Roman", part = "all") |>
    set_caption("Distribution of Observations by Industry") |>
    add_footer_lines(
        "* Observations represent total count of non-profits in each category."
    )

doc_industry <- body_add_flextable(doc_industry, value = flex_table_industry)

# Save the document to your Outputs folder
print(
    doc_industry,
    target = "../Outputs/1-Industry_Distribution_Table.docx"
)
