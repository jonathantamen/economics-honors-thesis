## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load-packages, message=FALSE, warning=FALSE------------------------------
# Load required packages
library(tidyverse) # For data manipulation
library(fixest) # For fixed effects regression (feols function)
library(knitr) # For nice tables
library(broom) # For tidy regression output
library(flextable) # For Word-ready tables


## ----load-data----------------------------------------------------------------
data <- readRDS("../Final_Data/final_data_set.rds")


## ----industry-heterogeneity---------------------------------------------------
# Get list of unique industries in your data (remove NAs)
industries <- unique(data$industry)
industries <- industries[!is.na(industries)] # Remove NA values

# Print how many industries we have
cat("Number of industries:", length(industries), "\n")

# Create an empty list to store regression results
industry_results <- list()

# Loop through each industry and run a regression
for (ind in industries) {
  # Print which industry we're working on (helps with debugging)
  cat("Processing industry:", ind, "\n")

  # Filter data for this specific industry
  industry_data <- data |>
    filter(industry == ind)

  # Check if we have enough data for this industry
  if (nrow(industry_data) < 10) {
    cat("  Skipping - not enough observations (n =", nrow(industry_data), ")\n")
    next # Skip to next industry
  }

  # Try to run the regression (with error handling)
  model <- tryCatch(
    {
      feols(
        log_total_program_expenses ~ gdp_change_percent + log_total_revenue | year + state + organization_ein,
        data = industry_data
      )
    },
    error = function(e) {
      cat("  Error for industry", ind, ":", e$message, "\n")
      return(NULL) # Return NULL if regression fails
    }
  )

  # Only store if model ran successfully
  if (!is.null(model)) {
    industry_results[[ind]] <- model
    cat("  Success - model stored\n")
  }
}

# Print how many successful regressions we got
cat("\nSuccessful regressions:", length(industry_results), "\n")


## ----display-results----------------------------------------------------------
# Create a summary table of coefficients for gdp_change_percent across industries
summary_table <- data.frame(
  Industry = character(),
  Coefficient = numeric(),
  Std_Error = numeric(),
  P_Value = numeric(),
  N_Obs = numeric(),
  stringsAsFactors = FALSE
)

# Extract key statistics from each regression
for (ind in names(industry_results)) {
  model <- industry_results[[ind]]

  # Skip if model is NULL
  if (is.null(model)) next

  # Get coefficient table
  coef_summary <- summary(model)$coeftable

  # Check if gdp_change_percent is in the model
  if ("gdp_change_percent" %in% rownames(coef_summary)) {
    summary_table <- rbind(summary_table, data.frame(
      Industry = ind,
      Coefficient = coef_summary["gdp_change_percent", "Estimate"],
      Std_Error = coef_summary["gdp_change_percent", "Std. Error"],
      P_Value = coef_summary["gdp_change_percent", "Pr(>|t|)"],
      N_Obs = model$nobs
    ))
  }
}

# SORTING STEP: Arrange by Coefficient Descending (Highest at top)
# This ensures the table order matches the visual order in the graph
summary_table <- summary_table |>
  arrange(desc(Coefficient))

# Display the table
kable(summary_table,
  digits = 4,
  caption = "GDP Change Percent Coefficient by Industry (Sorted by Effect Size)"
)


## ----visualize-heterogeneity, fig.width=10, fig.height=8----------------------
# Only create plot if we have results
if (nrow(summary_table) > 0) {
  # Create confidence intervals and significance indicator
  summary_table <- summary_table |>
    mutate(
      CI_lower = Coefficient - 1.96 * Std_Error,
      CI_upper = Coefficient + 1.96 * Std_Error,
      Significant = ifelse(P_Value < 0.05, "Yes", "No")
    )

  # LOCK ORDER: Set factor levels to match the sorted table.
  # We reverse() the order because ggplot's coord_flip() puts the last level at the top.
  # This ensures Table Row 1 (Highest) appears at the Top of the Graph.
  summary_table$Industry <- factor(summary_table$Industry, levels = rev(summary_table$Industry))

  # Create coefficient plot
  # Note: removed reorder() since we explicitly set levels above
  ggplot(summary_table, aes(x = Industry, y = Coefficient)) +
    geom_point(aes(color = Significant), size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
      width = 0.3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    labs(
      title = "Effect of GDP Change on Program Expenses by Industry",
      subtitle = "With 95% Confidence Intervals (Sorted by Magnitude)",
      x = "Industry",
      y = "Coefficient (Effect on Log Program Expenses)",
      color = "Significant at 5%"
    ) +
    scale_color_manual(values = c("No" = "gray60", "Yes" = "steelblue"))
} else {
  cat("No results to plot - check if regressions ran successfully\n")
}

## Exporting
# Image
ggsave(
  filename = "../Outputs/industry_coefficient_plot.png",
  plot = last_plot(),
  width = 10,
  height = 8,
  dpi = 300, # High resolution for Word
)
cat("PNG image saved: industry_coefficient_plot.png\n")


## -----------------------------------------------------------------------------
# Data Table
formatted_table <- summary_table |>
  mutate(
    # Add stars for significance
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  select(Industry, Coefficient, Std_Error, P_Value, Significance, N_Obs) |>
  flextable() |>
  set_header_labels(
    Industry = "Industry",
    Coefficient = "Coefficient",
    Std_Error = "Std. Error",
    P_Value = "P-Value",
    Significance = "Sig.",
    N_Obs = "N"
  ) |>
  colformat_double(j = c("Coefficient", "Std_Error", "P_Value"), digits = 4) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all")

save_as_docx(formatted_table, path = "../Outputs/6-industry_results_table.docx")
cat("Word table saved: industry_results_table.docx\n")


## -----------------------------------------------------------------------------
# 1. Determine base year (Year 1) revenue for each organization
base_revenue_data <- data |>
  group_by(organization_ein) |>
  filter(!is.na(total_revenue)) |>
  arrange(year) |>
  slice(1) |> # Take the first year available for each organization
  ungroup() |>
  select(organization_ein, base_revenue = total_revenue) |>
  mutate(
    # Calculate quintiles based on base year revenue
    revenue_quintile = ntile(base_revenue, 5),

    # Create descriptive labels for each quintile
    revenue_group = case_when(
      revenue_quintile == 1 ~ "Q1 (Smallest)",
      revenue_quintile == 2 ~ "Q2 (Small)",
      revenue_quintile == 3 ~ "Q3 (Medium)",
      revenue_quintile == 4 ~ "Q4 (Large)",
      revenue_quintile == 5 ~ "Q5 (Largest)",
      TRUE ~ NA_character_
    )
  )

# 2. Apply these fixed quintile groups to all years for each organization
revenue_data <- data |>
  left_join(
    base_revenue_data |> select(organization_ein, revenue_quintile, revenue_group),
    by = "organization_ein"
  )


## -----------------------------------------------------------------------------
# Check the base revenue ranges for each quintile and count of organizations
cat("Revenue Quintile Ranges (Based on Year 1 Revenue):\n")
base_revenue_data |>
  group_by(revenue_group) |>
  summarise(
    Min = min(base_revenue, na.rm = TRUE),
    Max = max(base_revenue, na.rm = TRUE),
    Median = median(base_revenue, na.rm = TRUE),
    N_Orgs = n()
  ) |>
  print()

# Check total observations per quintile in the full dataset
cat("\nTotal Observations across all years per quintile:\n")
revenue_data |>
  group_by(revenue_group) |>
  summarise(N_Obs = n()) |>
  print()


## -----------------------------------------------------------------------------
# Get list of quintiles
quintiles <- unique(revenue_data$revenue_quintile)
quintiles <- quintiles[!is.na(quintiles)] # Remove NAs
quintiles <- sort(quintiles) # Order from 1 to 5

# Create empty list to store results
quintile_results <- list()

# Loop through each quintile
for (q in quintiles) {
  cat("\nProcessing quintile:", q, "\n")

  # Filter data for this quintile
  quintile_data <- revenue_data |>
    filter(revenue_quintile == q)

  cat("  Observations:", nrow(quintile_data), "\n")

  # Check if we have enough data
  if (nrow(quintile_data) < 10) {
    cat("  Skipping - not enough observations\n")
    next
  }

  # Run regression with error handling
  model <- tryCatch(
    {
      feols(
        log_total_program_expenses ~ gdp_change_percent + log_total_revenue | year + state + organization_ein,
        data = quintile_data
      )
    },
    error = function(e) {
      cat("  Error:", e$message, "\n")
      return(NULL)
    }
  )

  # Store results if successful
  if (!is.null(model)) {
    # Use descriptive label as name
    quintile_label <- unique(quintile_data$revenue_group)[1]
    quintile_results[[quintile_label]] <- model
    cat("  Success - model stored\n")
  }
}

cat("\nSuccessful regressions:", length(quintile_results), "\n")


## ----display-quintile-results-------------------------------------------------
# Create summary table
quintile_summary <- data.frame(
  Quintile = character(),
  Coefficient = numeric(),
  Std_Error = numeric(),
  P_Value = numeric(),
  N_Obs = numeric(),
  stringsAsFactors = FALSE
)

# Extract coefficients
for (q_name in names(quintile_results)) {
  model <- quintile_results[[q_name]]

  if (is.null(model)) next

  coef_summary <- summary(model)$coeftable

  if ("gdp_change_percent" %in% rownames(coef_summary)) {
    quintile_summary <- rbind(quintile_summary, data.frame(
      Quintile = q_name,
      Coefficient = coef_summary["gdp_change_percent", "Estimate"],
      Std_Error = coef_summary["gdp_change_percent", "Std. Error"],
      P_Value = coef_summary["gdp_change_percent", "Pr(>|t|)"],
      N_Obs = model$nobs
    ))
  }
}

# Display table
kable(quintile_summary,
  digits = 4,
  caption = "GDP Effect by Organization Revenue Size (Quintiles)"
)


## ----visualize-quintile-results, fig.width=8, fig.height=6--------------------
# Create visualization if we have results
if (nrow(quintile_summary) > 0) {
  # Add confidence intervals
  quintile_summary <- quintile_summary |>
    mutate(
      CI_lower = Coefficient - 1.96 * Std_Error,
      CI_upper = Coefficient + 1.96 * Std_Error,
      Significant = ifelse(P_Value < 0.05, "Yes", "No"),
      # Ensure proper ordering
      Quintile = factor(Quintile, levels = c(
        "Q1 (Smallest)", "Q2 (Small)", "Q3 (Medium)",
        "Q4 (Large)", "Q5 (Largest)"
      ))
    )

  # Create plot
  ggplot(quintile_summary, aes(y = Coefficient, x = Quintile)) +
    geom_point(aes(color = Significant), size = 4) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant),
      width = 0.2, linewidth = 1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    labs(
      title = "GDP Effect on Program Expenses by Organization Size",
      subtitle = "Measured by Revenue Quintiles (95% Confidence Intervals)",
      x = "Revenue Quintile",
      y = "Coefficient (Effect on Log Program Expenses)",
      color = "Significant at 5%"
    ) +
    scale_color_manual(values = c("No" = "gray60", "Yes" = "steelblue"))
} else {
  cat("No results to plot\n")
}


## -----------------------------------------------------------------------------
# Revenue Results image
ggsave(
  filename = "../Outputs/revenue_quintile_plot.png",
  plot = last_plot(),
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)
cat("PNG saved: revenue_quintile_plot.png\n")


## -----------------------------------------------------------------------------
# Revenue Results data table
formatted_quintile_table <- quintile_summary |>
  mutate(
    # Add significance stars
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Add confidence intervals
    CI_lower = Coefficient - 1.96 * Std_Error,
    CI_upper = Coefficient + 1.96 * Std_Error,
    CI_95 = paste0("[", round(CI_lower, 4), ", ", round(CI_upper, 4), "]")
  ) |>
  select(Quintile, Coefficient, Std_Error, CI_95, P_Value, Significance, N_Obs) |>
  flextable() |>
  set_header_labels(
    Quintile = "Revenue Quintile",
    Coefficient = "Coefficient",
    Std_Error = "Std. Error",
    CI_95 = "95% CI",
    P_Value = "P-Value",
    Significance = "Sig.",
    N_Obs = "N"
  ) |>
  colformat_double(j = c("Coefficient", "Std_Error", "P_Value"), digits = 4) |>
  autofit() |>
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header")

# Save table as Word document
save_as_docx(formatted_quintile_table, path = "../Outputs/5-revenue_quintile_table.docx")
cat("Word table saved: revenue_quintile_table.docx\n")

print(formatted_quintile_table)
