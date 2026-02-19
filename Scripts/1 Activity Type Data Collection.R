## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)
options(scipen = 999)


## -----------------------------------------------------------------------------
activity_data_1 <- read_csv("../Raw_Data/IRS Activity Codes/eo1.csv") |>
  mutate(across(everything(), as.character))

activity_data_2 <- read_csv("../Raw_Data/IRS Activity Codes/eo2.csv") |>
  mutate(across(everything(), as.character))

activity_data_3 <- read_csv("../Raw_Data/IRS Activity Codes/eo3.csv") |>
  mutate(across(everything(), as.character))

# Combining
activity_data_raw <- bind_rows(activity_data_1, activity_data_2, activity_data_3)

## -----------------------------------------------------------------------------
# Now, filter to just EIN and activity code variables ()
activity_data_formatted <- activity_data_raw |>
  select(EIN, NTEE_CD, ACTIVITY, FOUNDATION) |>
  mutate(EIN = as.numeric(EIN))


## -----------------------------------------------------------------------------
library(dplyr)
# This function translates every activity_code (the old version) into new categories into a new column. # nolint
activity_data_formatted <- activity_data_formatted |> # Thanks, AI for writing the function. # nolint: line_length_linter.
  mutate(
    activity_code = as.numeric(substr(ACTIVITY, 1, 3))
  ) |>
  mutate(activity_category = case_when(
    !is.na(NTEE_CD) & NTEE_CD != "" ~ substr(NTEE_CD, 1, 1),
    # --- X: Religion-Related ---
    activity_code >= 1 & activity_code <= 29 ~ "X", # double-checked.
    # --- B: Education ---
    activity_code >= 30 & activity_code <= 59 ~ "B", # double-checked.
    # --- A: Arts, Culture & Humanities ---
    activity_code >= 60 & activity_code <= 149 ~ "A", # double-checked.
    # --- E: Health Care (With Exceptions) ---
    activity_code == 166 ~ "F", # double-checked.
    activity_code %in% c(161, 162) ~ "H", # double-checked.
    activity_code >= 150 & activity_code <= 179 ~ "E", # double-checked.
    # --- U: Science & Technology ---
    activity_code >= 180 & activity_code <= 199 ~ "U", # double-checked.
    # --- S: Community Improvement ---
    activity_code >= 200 & activity_code <= 229 ~ "S", # double-checked.
    # --- K: Food, Agriculture & Nutrition ---
    activity_code >= 230 & activity_code <= 249 ~ "K", # double-checked.
    # --- Y: Mutual & Membership Benefit ---
    activity_code >= 250 & activity_code <= 279 ~ "Y", # double-checked.
    # --- N: Recreation & Sports ---
    activity_code >= 280 & activity_code <= 319 ~ "N", # double-checked.
    # --- O: Youth Development ---
    activity_code >= 320 & activity_code <= 349 ~ "O", # double-checked.
    # --- C: Environment (With Exception) ---
    activity_code == 355 ~ "D", # double-checked.
    activity_code >= 350 & activity_code <= 379 ~ "C", # double-checked.
    # --- L: Housing & Shelter ---
    # Housing (380-399)
    activity_code >= 380 & activity_code <= 399 ~ "L", # double-checked.
    # --- S: Community Improvement (Inner City) ---
    activity_code == 406 ~ "I", # double-checked.
    activity_code == 407 ~ "M", # double-checked.
    # Inner City/Community (400-429) -> S
    activity_code >= 400 & activity_code <= 429 ~ "S", # double-checked.
    # --- R: Civil Rights & Advocacy ---
    # Civil Rights (430-459)
    activity_code >= 430 & activity_code <= 459 ~ "R", # double-checked.
    # --- I: Crime & Legal-Related ---
    # Litigation (460-479)
    activity_code >= 460 & activity_code <= 479 ~ "I", # double-checked.
    # --- R: Political/Legislative ---
    # Legislative (480-509)
    activity_code >= 480 & activity_code <= 559 ~ "R", # double-checked.
    # --- P: Human Services (With Exception) ---
    # 573 is Rehab/Substance Abuse -> F (Mental Health)
    activity_code == 573 ~ "F", # double-checked.
    activity_code == 572 ~ "I", # double-checked.
    # Services to Individuals (560-579) -> P
    activity_code >= 560 & activity_code <= 575 ~ "P", # double-checked.
    # --- T: Philanthropy ---
    # Grantmaking (600-699)
    activity_code >= 600 & activity_code <= 699 ~ "T", # double-checked.
    # --- Misc/Other (900+) ---
    activity_code == 900 ~ "Y", # Cemetery #double-checked.
    activity_code == 907 | activity_code == 908 ~ "W", # Veterans/Patriotic -> Public Benefit #double-checked.
    activity_code == 910 | activity_code == 911 ~ "Q", # Foreign #double-checked.
    activity_code == 913 ~ "D", # Animal Cruelty #double-checked.
    activity_code == 927 ~ "T", # Fundraising #double-checked.
    # Default catch-all for any remaining codes
    TRUE ~ "Z"
  ))


## -----------------------------------------------------------------------------
# This function converts the new activity categories into more easily understood string values, while maintaining data efficiency by using factor() data format. # nolint: line_length_linter.
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
  )) |>
  mutate(activity_category = factor(activity_category))


## -----------------------------------------------------------------------------
# Now, I want to turn FOUNDATION column into a factor for only if organization is a school, church, or neither.
# Add this after line 31 (after combining the datasets)

activity_data_formatted <- activity_data_formatted |>
  mutate(org_type = case_when(
    FOUNDATION == 10 ~ "Church",
    FOUNDATION == 11 ~ "School",
    TRUE ~ "Neither"
  )) |>
  # Convert to factor with specified levels
  mutate(org_type = factor(org_type, levels = c("School", "Church", "Neither")))


## -----------------------------------------------------------------------------
# First, I can drop the old columns I no longer need..
activity_data_formatted <- activity_data_formatted |>
  select(-c(
    ACTIVITY, NTEE_CD, activity_code, FOUNDATION
  )) |>
  mutate( # Improving data storage efficiency
    EIN = as.integer(EIN),
    activity_category = factor(activity_category)
  )

## -----------------------------------------------------------------------------
saveRDS(activity_data_formatted, "../Final_Data/activity_data.rds")
