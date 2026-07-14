# ==============================================================================
# STEP 1c: State GDP Data Collection & Percent Change Calculation
# Economics Honors Thesis Data Pipeline
# ==============================================================================
# This script imports annual state-level real GDP data from the Bureau of
# Economic Analysis (BEA), reshapes it from wide to long format, and calculates
# the annual percentage growth rate of GDP for each state.
# ==============================================================================

# Setup environment options
knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999) # Disable scientific notation

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)

# ==============================================================================
# 1. Load Raw State GDP Data
# This dataset is downloaded from the Bureau of Economic Analysis (BEA) SAGDP1 tables.
# ==============================================================================
gdp_data <- read_csv("/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Raw_Data/SAGDP/SAGDP1__ALL_AREAS_1997_2024.csv", show_col_types = FALSE)

# ==============================================================================
# 2. Reshape and Calculate Annual GDP growth rates
# ==============================================================================
gdp_data <- gdp_data |>
  # Filter only for Real GDP in millions of chained 2017 dollars
  filter(Description == "Real GDP (millions of chained 2017 dollars) 1/") |>
  # Keep state name and the years of interest (2008 to 2023)
  # (We include 2008 so that the percentage change for 2009 can be computed using lag)
  select(
    state = GeoName,
    `2008`:`2023`
  ) |>
  # Pivot the years from wide columns into a long format
  # This makes it a panel dataset: State | Year | Real GDP
  pivot_longer(
    cols = `2008`:`2023`,
    names_to = "YEAR",
    values_to = "real_gdp"
  ) |>
  # Standardize variable types
  mutate(
    YEAR = as.integer(YEAR),
    real_gdp = as.integer(real_gdp)
  ) |>
  # Ensure the dataset is ordered alphabetically by state and chronologically by year
  arrange(state, YEAR) |>
  # CRITICAL CORRECTION: group_by(state) ensures that the lag() function is computed
  # within each state. Without this, the lag for the first year of a new state in the
  # list (e.g. 2008 for Alabama) would reference the last year of the preceding state.
  group_by(state) |>
  mutate(
    gdp_change_percent = ((real_gdp - lag(real_gdp)) / lag(real_gdp)) * 100
  ) |>
  ungroup() # Always ungroup after group-based mutations to prevent unexpected behavior later

# ==============================================================================
# 3. Export Processed GDP Data
# ==============================================================================
saveRDS(gdp_data, "../Final_Data/gdp_data.rds")
