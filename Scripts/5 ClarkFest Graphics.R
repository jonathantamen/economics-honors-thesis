# ----------5. ClarkFest Graphics----------
# This script is dedicated to generating high-quality graphics for the poster.

# 1. Load Packages
library(dplyr)
library(ggplot2)

# 2. Load Data
data <- readRDS("../Final_Data/final_data_set.rds")

# 3. Setup Poster Theme
# Set blue and red as the default global drawing colors for all lines/bars
options(ggplot2.discrete.colour = c("#1f78b4", "#e31a1c"))
options(ggplot2.discrete.fill = c("#1f78b4", "#e31a1c"))

# Having a unified theme ensures all your graphics look like they belong together
theme_clarkfest <- function() {
  theme_minimal() +
    theme(
      # Using "serif" gives a Times-like font that works natively in R plots
      text = element_text(family = "serif"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank() # Keep it clean by removing minor grid lines
    )
}

# ----------Graphic 1: Cyclical Trends (State GDP vs. Program Expenses)----------

# Step 1: Data Prep
# We want the average of the % change in program expenses compared to average GDP % change.
graphic1_data <- data |>
  arrange(organization_ein, year) |>
  group_by(organization_ein) |>
  # 1. Calculate percentage change for each non-profit year-over-year
  mutate(exp_pct_change = (total_program_expenses - lag(total_program_expenses)) / lag(total_program_expenses) * 100) |>
  ungroup() |>
  # 2. Filter out extreme mathematical outliers (e.g. going from $1 to $10,000)
  # so that the strict mathematical mean is not completely distorted by infinite values or division by zero.
  # We cap the change to a reasonable threshold for macro averaging (-100% to +500%).
  filter(is.finite(exp_pct_change), exp_pct_change >= -100, exp_pct_change <= 500) |>
  group_by(year) |>
  # 3. Take the averages for each year
  summarize(
    mean_gdp = mean(gdp_change_percent, na.rm = TRUE),
    mean_exp_change = mean(exp_pct_change, na.rm = TRUE)
  ) |>
  filter(!is.na(mean_gdp), !is.na(mean_exp_change))

# Step 2: Create Plot
plot1 <- ggplot(data = graphic1_data, aes(x = year)) +
  # Add a bold line at 0% for baseline reference
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  # GDP Line
  geom_line(aes(y = mean_gdp, color = "Average GDP Growth (%)"), linewidth = 1.2) +
  # Program Expenses % Change Line
  geom_line(aes(y = mean_exp_change, color = "Average Program Exp Growth (%)"), linewidth = 1.2) +

  # Because both are now percentage rates, they can cleanly share the same Y axis!
  scale_y_continuous(name = "Annual Percentage Rate (%)") +
  scale_x_continuous(breaks = seq(min(graphic1_data$year, na.rm = TRUE), max(graphic1_data$year, na.rm = TRUE), by = 2)) +

  # Apply our clean poster theme
  theme_clarkfest() +
  labs(
    title = "Cyclical Trends: State GDP vs. Program Expenses",
    subtitle = "Comparing Average State GDP Growth to Average Program Expense Growth (2009-2023)",
    x = "Year",
    color = "Variables:"
  )

# Step 3: Preview Plot in R
print(plot1)

# Step 4: Save Output
# Make sure the Outputs folder exists (it should based on your previous scripts)
ggsave(
  filename = "../Outputs/ClarkFest_Graphic_1_Cyclical.png",
  plot = plot1,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Graphic 2: Top 5 Non-Profits by Expense (Distinct Industries)
# ------------------------------------------------------------------------------

# Step 1: Data Prep
graphic2_data <- data |>
  # We want the snapshot of the most recent data
  filter(year == 2023) |>
  group_by(industry) |>
  # Grab the single largest non-profit (by expenses) inside each distinct industry
  slice_max(order_by = total_expenses, n = 1, with_ties = FALSE) |>
  ungroup() |>
  # From those industry leaders, slice the top 5 overall highest spenders
  slice_max(order_by = total_expenses, n = 5, with_ties = FALSE) |>
  # Select your 4 requested columns and rename them cleanly for the presentation
  select(
    `Organization Name` = organization_name,
    `Sector/Industry` = industry,
    `State HQ` = state,
    `Total Expenses (2023)` = total_expenses
  )

# Step 2: Create Table
library(flextable)
library(scales) # Loaded for the dollar() function

graphic2_table <- flextable(graphic2_data) |>
  # This makes the columns snap to the perfect width based on the text length
  autofit() |>
  # Cleanly formats raw numerical expenses into e.g. "$74,356,004,001"
  set_formatter(`Total Expenses (2023)` = function(x) scales::dollar(x)) |>
  # Matches the professional styling you used in your regression script
  theme_vanilla() |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header") |>
  add_header_lines("Top 5 Non-Profits by Expenditure (Diverse Industries, 2023)") |>
  align(align = "center", part = "header")

# Step 3: Preview Table in R Studio Viewer
print(graphic2_table)

# Step 4: Save Output
# Saving directly into Word Document formats so you can copy and paste
# natively into a poster template without blurry image resolution.
save_as_docx(graphic2_table, path = "../Outputs/ClarkFest_Graphic_2_Top5.docx")

# ------------------------------------------------------------------------------
# Graphic 3: Non-Profit Observations vs. Average GDP
# ------------------------------------------------------------------------------

# Step 1: Data Prep
graphic3_data <- data |>
  group_by(year) |>
  summarize(
    num_orgs = n(), # Counts the number of observations (rows) per year
    mean_gdp = mean(gdp_change_percent, na.rm = TRUE)
  ) |>
  filter(!is.na(mean_gdp))

# Step 2: Create Plot
# Mathematical Scaling: GDP is between -3 and +6, but Orgs go up to 336,000!
# We multiply GDP by 40,000 so a 6% GDP maps to 240,000 on the Y-axis perfectly overlapping the bars.
gdp_scale_factor <- 40000

plot3 <- ggplot(graphic3_data, aes(x = year)) +
  # 1. Bar Chart: Blue bars for the number of orgs
  geom_bar(aes(y = num_orgs), stat = "identity", fill = "#1f78b4", alpha = 0.8) +

  # 2. Line Graph: Red line for the Average GDP
  # We scale the GDP up so it plots visibly on the same coordinate plane as the massive bar counts
  geom_line(aes(y = mean_gdp * gdp_scale_factor), color = "#e31a1c", linewidth = 1.2) +
  geom_point(aes(y = mean_gdp * gdp_scale_factor), color = "#e31a1c", size = 2) +

  # Add the labels for the Bar Chart safely ON TOP of the graphics
  # We use geom_label with a translucent white background so the line never crashes through the text!
  geom_label(aes(y = num_orgs, label = scales::comma(num_orgs)),
    vjust = -0.5, size = 3, color = "black", family = "serif",
    fill = alpha("white", 0.7), label.size = NA
  ) +

  # Add the labels for the GDP Line
  # Placed slightly below the nodes (using vjust = 1.5) so they don't collide with the bar labels
  geom_label(aes(y = mean_gdp * gdp_scale_factor, label = paste0(round(mean_gdp, 1), "%")),
    vjust = 1.5, size = 3, color = "black", family = "serif",
    fill = alpha("white", 0.7), label.size = NA
  ) +

  # 3. Setup the Dual Y-Axis
  scale_y_continuous(
    name = "Number of Non-Profits Observed",
    labels = scales::comma,
    # The secondary axis reverses the scaling math so the viewer sees the true GDP %
    sec.axis = sec_axis(~ . / gdp_scale_factor, name = "Average GDP Growth (%)")
  ) +
  scale_x_continuous(breaks = seq(min(graphic3_data$year), max(graphic3_data$year), by = 2)) +
  theme_clarkfest() +
  labs(
    title = "Number of Non-profits Observed each Year (Bar) vs. Average GDP (Line)",
    x = "Year"
  )

# Step 3: Preview in R Studio
print(plot3)

# Step 4: Save Output
ggsave(
  filename = "../Outputs/ClarkFest_Graphic_3_Observations.png",
  plot = plot3,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Graphic 4: Log Distributions of Expenses vs. Revenue (Side-by-Side Histogram)
# ------------------------------------------------------------------------------

# Step 1: Data Prep
# To put bars side-by-side natively in ggplot, we reshape the data using tidyr::pivot_longer
# so both revenues and expenses live in a single "Value" column tagged by a "Metric" column.
graphic4_data <- data |>
  select(log_total_expenses, log_total_revenue) |>
  # Eliminate any infinities caused by log(0)
  filter(is.finite(log_total_expenses), is.finite(log_total_revenue)) |>
  tidyr::pivot_longer(
    cols = c(log_total_expenses, log_total_revenue),
    names_to = "Metric",
    values_to = "Log_Value"
  ) |>
  # Clean up the names so they look professional in our legend!
  mutate(Metric = ifelse(Metric == "log_total_revenue", "Log Revenue", "Log Expenses"))

# Step 2: Create Plot
plot4 <- ggplot(graphic4_data, aes(x = Log_Value, fill = Metric)) +
  # geom_histogram creates bar charts for distributions. 
  geom_histogram(bins = 40, color = "white", alpha = 0.9) +
  
  # facet_wrap splits the data natively into two totally separate side-by-side charts!
  facet_wrap(~ Metric) +
  
  # Ensure the fill colors explicitly match the global Blue/Red theme you selected
  scale_fill_manual(values = c("Log Revenue" = "#1f78b4", "Log Expenses" = "#e31a1c")) +
  
  # Format axes titles
  scale_x_continuous(name = "Log Value (Dollars)") +
  # Histograms measure literal counts (how many non-profits) instead of density percentages
  scale_y_continuous(name = "Count of Observations", labels = scales::comma) +
  
  # Apply our clean poster theme
  theme_clarkfest() +
  labs(
    title = "Macro Distribution: Revenue Details vs. Expenditures",
    subtitle = "Side-by-Side Bar Chart of Non-Profit Finances (Log Scale)",
    fill = "Metric:"
  )

# Step 3: Preview Plot in R
print(plot4)

# Step 4: Save Output
ggsave(
  filename = "../Outputs/ClarkFest_Graphic_4_Histograms.png",
  plot = plot4,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Graphic 5: Organizations Growing Program Expenses (Year-Over-Year)
# ------------------------------------------------------------------------------

# Step 1: Data Prep
# We calculate both the 'Growing' count and the 'Total' count, and reshape it for side-by-side plotting.
graphic5_data <- data |>
  arrange(organization_ein, year) |>
  group_by(organization_ein) |>
  # Determine if expenses are strictly greater than the previous year
  mutate(grew_expenses = total_program_expenses > lag(total_program_expenses)) |>
  ungroup() |>
  # Remove NAs (the very first year an org appears cannot be measured for 'growth')
  filter(!is.na(grew_expenses)) |>
  group_by(year) |>
  summarize(
    `Growing Organizations` = sum(grew_expenses),
    `Total Organizations` = n()
  ) |>
  tidyr::pivot_longer(
    cols = c(`Total Organizations`, `Growing Organizations`),
    names_to = "Metric",
    values_to = "Count"
  ) |>
  # Force Total Organizations to always render as the first bar on the left
  mutate(Metric = factor(Metric, levels = c("Total Organizations", "Growing Organizations")))

# Step 2: Create Plot
plot5 <- ggplot(graphic5_data, aes(x = year, y = Count, fill = Metric)) +

  # Highlight the 2019-2020 shock natively before drawing the bars so it sits cleanly in the background behind them!
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 0, ymax = Inf, fill = "gray", alpha = 0.3) +
  # Add slightly elevated text inside the rectangle indicating what it represents
  annotate("text", x = 2020, y = max(graphic5_data$Count) * 0.95, label = "Pandemic\nShock", family = "serif", fontface = "italic", size = 4) +
  
  # Bars are plotted 'dodged' so Total and Growing sit nicely shoulder-to-shoulder
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.9) +
  
  # Ensure the fill colors explicitly match the global Red/Blue theme
  scale_fill_manual(values = c("Total Organizations" = "#1f78b4", "Growing Organizations" = "#e31a1c")) +
  
  # Add exact labels securely above the bars
  geom_text(aes(label = scales::comma(Count)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2.5, color = "black", family = "serif") +
  
  # Format Axes cleanly
  scale_x_continuous(breaks = seq(min(graphic5_data$year), max(graphic5_data$year), by = 1)) +
  # Expand the top of the Y axis slightly so the highest text labels don't get clipped off the edge
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  
  # Apply Theme
  theme_clarkfest() +
  labs(
    title = "Non-Profit Resilience: Organizations Growing Expenses vs. Total",
    subtitle = "Tracking Overall Sample Size vs. Organizations showing Year-Over-Year Expense Growth",
    x = "Year",
    y = "Number of Organizations",
    fill = "Metric:"
  )

# Step 3: Preview in R Studio
print(plot5)

# Step 4: Save Output
ggsave(
  filename = "../Outputs/ClarkFest_Graphic_5_GrowthHighlight.png",
  plot = plot5,
  width = 8,
  height = 6,
  dpi = 300
)
