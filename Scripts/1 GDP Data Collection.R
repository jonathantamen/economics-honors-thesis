## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Scripts")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)
options(scipen = 999)


## -----------------------------------------------------------------------------
gdp_data <- read_csv("/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Raw_Data/SAGDP/SAGDP1__ALL_AREAS_1997_2024.csv")

gdp_data <- gdp_data |>
  filter(Description == "Real GDP (millions of chained 2017 dollars) 1/") |>
  select(
    state = GeoName,
    `2008`:`2023`
  ) |>
  # Now to turn it into long data format
  pivot_longer(
    cols = `2008`:`2023`,
    names_to = "YEAR",
    values_to = "real_gdp"
  ) |>
  # Now to do little data cleanup (changing type) and creating percent change column
  mutate(
    YEAR = as.integer(YEAR),
    real_gdp = as.integer(real_gdp),
    gdp_change_percent = ((real_gdp - lag(real_gdp)) / lag(real_gdp)) * 100
  )


## -----------------------------------------------------------------------------
# 6. Saving
saveRDS(gdp_data, "../Final_Data/gdp_data.rds")
