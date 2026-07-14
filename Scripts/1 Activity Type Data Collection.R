# ==============================================================================
# STEP 1b: Industry & Organization Type Mapping (IRS Activity & NTEE Codes)
# Economics Honors Thesis Data Pipeline
# ==============================================================================
# This script processes the IRS Business Master Files (exempt organizations data)
# to classify non-profit organizations into industry categories using NTEE
# (National Taxonomy of Exempt Entities) codes. It also extracts the organization's
# foundation status to identify schools and churches.
# ==============================================================================

# Setup environment options
knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999) # Disable scientific notation (makes EIN formatting stable)

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)

# ==============================================================================
# 1. Load Raw IRS Exempt Organizations Business Master Files (EO BMF)
# The data is divided into three separate CSV files based on regions.
# We read all columns as characters to prevent formatting mismatches before merging.
# ==============================================================================
activity_data_1 <- read_csv("../Raw_Data/IRS Activity Codes/eo1.csv") |>
  mutate(across(everything(), as.character))

activity_data_2 <- read_csv("../Raw_Data/IRS Activity Codes/eo2.csv") |>
  mutate(across(everything(), as.character))

activity_data_3 <- read_csv("../Raw_Data/IRS Activity Codes/eo3.csv") |>
  mutate(across(everything(), as.character))

# Combine regional data frames vertically into a single raw master set
activity_data_raw <- bind_rows(activity_data_1, activity_data_2, activity_data_3)

# ==============================================================================
# 2. Select Identifiers & Key Variables
# Select only:
#   - EIN: Unique organization ID (converted to numeric for joining)
#   - NTEE_CD: Modern IRS sector code (e.g. "A30" for libraries)
#   - ACTIVITY: Historical 3-digit numeric activity code
#   - FOUNDATION: Numerical code indicating foundation class (school, church, etc.)
# ==============================================================================
activity_data_formatted <- activity_data_raw |>
  select(EIN, NTEE_CD, ACTIVITY, FOUNDATION) |>
  mutate(EIN = as.numeric(EIN))

# ==============================================================================
# 3. Translate Historical Activity Codes to Modern NTEE Categories
# The IRS transition to NTEE classification left some historical filings classified
# only by a 3-digit activity code. This section bridges the gap:
#   - If a modern NTEE_CD exists, we use its first letter (the major category).
#   - If only the 3-digit ACTIVITY code exists, we map it to its corresponding NTEE letter.
# ==============================================================================
activity_data_formatted <- activity_data_formatted |>
  mutate(
    # Extract first 3 digits of the historical activity code
    activity_code = as.numeric(substr(ACTIVITY, 1, 3))
  ) |>
  mutate(activity_category = case_when(
    # If NTEE code is already present, extract its first letter (broad category)
    !is.na(NTEE_CD) & NTEE_CD != "" ~ substr(NTEE_CD, 1, 1),
    
    # --- Category Mapping based on historic IRS activity codes ---
    # Religion-Related (Category X)
    activity_code >= 1 & activity_code <= 29 ~ "X",
    # Education (Category B)
    activity_code >= 30 & activity_code <= 59 ~ "B",
    # Arts, Culture & Humanities (Category A)
    activity_code >= 60 & activity_code <= 149 ~ "A",
    # Health Care / Mental Health (Category E/F/H)
    activity_code == 166 ~ "F", # Crisis Intervention / Mental Health
    activity_code %in% c(161, 162) ~ "H", # Medical Research
    activity_code >= 150 & activity_code <= 179 ~ "E", # Health Care General
    # Science & Technology (Category U)
    activity_code >= 180 & activity_code <= 199 ~ "U",
    # Community Improvement (Category S)
    activity_code >= 200 & activity_code <= 229 ~ "S",
    # Food, Agriculture & Nutrition (Category K)
    activity_code >= 230 & activity_code <= 249 ~ "K",
    # Mutual & Membership Benefit (Category Y)
    activity_code >= 250 & activity_code <= 279 ~ "Y",
    # Recreation & Sports (Category N)
    activity_code >= 280 & activity_code <= 319 ~ "N",
    # Youth Development (Category O)
    activity_code >= 320 & activity_code <= 349 ~ "O",
    # Environment & Animal Protection (Category C/D)
    activity_code == 355 ~ "D", # Animal Protection
    activity_code >= 350 & activity_code <= 379 ~ "C", # Environment General
    # Housing & Shelter (Category L)
    activity_code >= 380 & activity_code <= 399 ~ "L",
    # Public Safety & Community (Category I/M/S)
    activity_code == 406 ~ "I", # Crime/Legal
    activity_code == 407 ~ "M", # Public Safety
    activity_code >= 400 & activity_code <= 429 ~ "S", # General Community Improvement
    # Civil Rights & Advocacy (Category R)
    activity_code >= 430 & activity_code <= 459 ~ "R",
    # Litigation & Legal Aid (Category I)
    activity_code >= 460 & activity_code <= 479 ~ "I",
    # Political/Legislative (Category R)
    activity_code >= 480 & activity_code <= 559 ~ "R",
    # Human Services / Mental Health / Law (Category P/F/I)
    activity_code == 573 ~ "F", # Rehab/Substance Abuse -> Mental Health
    activity_code == 572 ~ "I", # Delinquency Prevention -> Law/Legal
    activity_code >= 560 & activity_code <= 575 ~ "P", # Human Services General
    # Philanthropy & Voluntarism (Category T)
    activity_code >= 600 & activity_code <= 699 ~ "T",
    # Miscellaneous Exceptions
    activity_code == 900 ~ "Y", # Cemeteries -> Mutual Benefit
    activity_code == 907 | activity_code == 908 ~ "W", # Veterans/Patriotic -> Public Benefit
    activity_code == 910 | activity_code == 911 ~ "Q", # International Affairs
    activity_code == 913 ~ "D", # Prevention of Cruelty to Animals -> Animal-Related
    activity_code == 927 ~ "T", # Fundraising Organizations -> Philanthropy
    # Catch-all default
    TRUE ~ "Z"
  ))

# ==============================================================================
# 4. Map Broad Letters to Human-Readable Sector Labels
# Factorizing these categories optimizes memory and ensures they are recognized
# as categorical variables in downstream plots and regressions.
# ==============================================================================
activity_data_formatted <- activity_data_formatted |>
  mutate(activity_category = case_when(
    activity_category == "A" ~ "Arts, Culture & Humanities",
    activity_category == "B" ~ "Education",
    activity_category == "C" ~ "Environment",
    activity_category == "D" ~ "Animal-Related",
    activity_category == "E" ~ "Health Care",
    activity_category == "F" ~ "Mental Health & Crisis Intervention",
    activity_category == "G" ~ "Voluntary Health Associations & Medical Disciplines",
    activity_category == "H" ~ "Medical Research",
    activity_category == "I" ~ "Crime & Legal-Related",
    activity_category == "J" ~ "Employment",
    activity_category == "K" ~ "Food, Agriculture & Nutrition",
    activity_category == "L" ~ "Housing & Shelter",
    activity_category == "M" ~ "Public Safety",
    activity_category == "N" ~ "Recreation & Sports",
    activity_category == "O" ~ "Youth Development",
    activity_category == "P" ~ "Human Services",
    activity_category == "Q" ~ "International, Foreign Affairs",
    activity_category == "R" ~ "Civil Rights, Social Action & Advocacy",
    activity_category == "S" ~ "Community Improvement & Capacity Building",
    activity_category == "T" ~ "Philanthropy, Voluntarism & Grantmaking",
    activity_category == "U" ~ "Science & Technology",
    activity_category == "V" ~ "Social Science",
    activity_category == "W" ~ "Public & Societal Benefit",
    activity_category == "X" ~ "Religion-Related",
    activity_category == "Y" ~ "Mutual & Membership Benefit",
    activity_category == "Z" ~ "Unknown or Unclassified",
    TRUE ~ "Unknown or Unclassified"
  )) |>
  mutate(activity_category = factor(activity_category))

# ==============================================================================
# 5. Extract Organization Type (Foundation Status)
# We isolate schools and churches using their IRS Foundation codes:
#   - Code 10: Church or association of churches
#   - Code 11: School
# This lets us exclude them from our final regression models later,
# as they have very different financing/operational structures than standard charities.
# ==============================================================================
activity_data_formatted <- activity_data_formatted |>
  mutate(org_type = case_when(
    FOUNDATION == 10 ~ "Church",
    FOUNDATION == 11 ~ "School",
    TRUE ~ "Neither"
  )) |>
  # Convert to a factor with specified reference levels
  mutate(org_type = factor(org_type, levels = c("School", "Church", "Neither")))

# ==============================================================================
# 6. Final Clean & Save
# Drop internal parsing/temporary columns to keep the dataset size minimal.
# ==============================================================================
activity_data_formatted <- activity_data_formatted |>
  select(-c(ACTIVITY, NTEE_CD, activity_code, FOUNDATION)) |>
  mutate(
    EIN = as.integer(EIN), # Explicitly coerce to integer to reduce memory footprint
    activity_category = factor(activity_category)
  )

# Export the processed dataset
saveRDS(activity_data_formatted, "../Final_Data/activity_data.rds")
