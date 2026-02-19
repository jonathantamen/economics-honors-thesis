## ----setup, include=FALSE-----------------------------------------------------
#Document setup
knitr::opts_chunk$set(echo = TRUE)
options(scipen = 999)

## -----------------------------------------------------------------------------
#Imports
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)

## -----------------------------------------------------------------------------
#Data import
org_data = readRDS(
  "../Final_Data/clean_master_data.rds"
  )


## -----------------------------------------------------------------------------
# number of observations, divided per year
unique_returns_by_year = org_data %>%
  # 1. Group the data by the Year column (using your specific column name)
  group_by(YEAR) %>%
  # 2. Summarize by counting the DISTINCT (unique) ORG_EIN values
  summarise(unique_returns = n_distinct(ORG_EIN))

plot_returns = ggplot(data= unique_returns_by_year, aes(x = factor(YEAR), y = unique_returns))+
  geom_col(fill = "purple") +
  geom_text(aes(label = unique_returns), vjust = -0.5) +
  labs(
    title = "Total number of Form 990 Returns",
    subtitle = "Summarized by year, from 2009-2023",
    y = "Number of returns",
    x = "Year"
  )
print(plot_returns)


## -----------------------------------------------------------------------------
ggsave("../Outputs/total_returns.png", plot = plot_returns)



## -----------------------------------------------------------------------------
key_variables = org_data %>% 
  select(
    # Demand side variables
    F9_09_EXP_GRANT_US_ORG_TOT,
    F9_09_EXP_GRANT_US_INDIV_TOT,
    F9_09_EXP_TOT_PROG,
    
    # Supply side variables
    F9_08_REV_CONTR_FED_CAMP, 
    F9_08_REV_CONTR_FUNDR_EVNT, 
    F9_08_REV_CONTR_GOVT_GRANT, 
    F9_08_REV_CONTR_MEMBSHIP_DUE, 
    F9_08_REV_PROG_TOT_TOT,
    
    #Fixed effect variables
    #F9_00_ORG_ADDR_STATE,
    #F9_00_YEAR_FORMATION,
    #F9_03_PROG_CODE
  )

# Temporary code fix to remove negatives in a column I forgot about.
key_variables = key_variables %>%
  filter(if_all(
    .cols = all_of("F9_08_REV_PROG_TOT_TOT"),
    .fns = ~ .x >= 0
  ))


## -----------------------------------------------------------------------------
library(psych)
summary_table = describe(key_variables)
print(summary_table)

## -----------------------------------------------------------------------------
library(dplyr)

summary_table <- key_variables %>%
  summarise(across(
    everything(),
    list(
      N = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    )
  ))


## -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)

summary_table_tidy <- summary_table %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )



## -----------------------------------------------------------------------------
summary_table_tidy <- summary_table_tidy %>%
  mutate(variable = gsub("^F9_", "", variable))


## -----------------------------------------------------------------------------
library(modelsummary)


datasummary_df(
  summary_table_tidy,
  output = "../Outputs/summary_statistics.docx"
)



## -----------------------------------------------------------------------------
org_data <- org_data %>% arrange(ORG_EIN, YEAR)

# Step 2: Calculate the distribution of MAX consecutive zero-expense years
consecutive_dist <- org_data %>%
  filter(YEAR <= 2020) %>% # I want to keep the Covid effect
  group_by(ORG_EIN) %>%
  summarize(max_consecutive_zeros = {
    runs <- rle(F9_09_EXP_TOT_PROG == 0)
    # Identify the maximum length where the value is TRUE (i.e., it was a zero)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  count(max_consecutive_zeros)

print(consecutive_dist)


## -----------------------------------------------------------------------------
#Visualization
org_data %>%
  group_by(ORG_EIN) %>%
  summarize(max_streak = {
    runs <- rle(F9_09_EXP_TOT_PROG == 0)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  ggplot(aes(x = factor(max_streak))) +
  geom_bar(fill = "brown") +
  labs(title = "Distribution of Consecutive Zero-Expense Years",
       x = "Longest Consecutive Streak of $0 Expenses",
       y = "Number of Organizations") +
  theme_minimal()


## -----------------------------------------------------------------------------
#Data summary of revenue
summary(org_data$F9_08_REV_TOT_TOT)

## -----------------------------------------------------------------------------
#Data summary of log(revenue)
revenue = org_data$F9_08_REV_TOT_TOT
log_revenue = log(revenue + 1)
summary(log_revenue)

## -----------------------------------------------------------------------------
#Visualization of revenue and log(revenue). Thanks, AI.
library(patchwork) # To put plots side-by-side

# Plot A: Raw Revenue (likely heavily skewed)
p1 <- ggplot(org_data, aes(x = revenue)) +
  geom_histogram(fill = "steelblue", bins = 50) +
  labs(title = "Raw Revenue", x = "Dollars ($)", y = "Count") +
  theme_minimal()

# Plot B: Log Revenue (likely more bell-shaped/normal)
p2 <- ggplot(org_data, aes(x = log_revenue)) +
  geom_histogram(fill = "darkorange", bins = 50) +
  labs(title = "Log-Transformed Revenue", x = "Log Value", y = "Count") +
  theme_minimal()

# Combine them
p1 + p2


## -----------------------------------------------------------------------------
org_data <- org_data %>% arrange(ORG_EIN, YEAR)

# Step 2: Calculate the distribution of MAX consecutive zero-expense years
consecutive_dist_rev <- org_data %>%
  filter(YEAR <= 2020) %>% # I want to keep the Covid effect
  group_by(ORG_EIN) %>%
  summarize(max_consecutive_rev_zeros = {
    runs <- rle(F9_08_REV_TOT_TOT == 0)
    # Identify the maximum length where the value is TRUE (i.e., it was a zero)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  count(max_consecutive_rev_zeros)

print(consecutive_dist_rev)

## -----------------------------------------------------------------------------
#Visualization
org_data %>%
  group_by(ORG_EIN) %>%
  summarize(max_streak = {
    runs <- rle(F9_08_REV_TOT_TOT == 0)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  ggplot(aes(x = factor(max_streak))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribution of Consecutive Zero-Revenue Years",
       x = "Longest Consecutive Streak of $0 Revenues",
       y = "Number of Organizations") +
  theme_minimal()


## -----------------------------------------------------------------------------
org_data <- org_data %>% arrange(ORG_EIN, YEAR)
# Step 2: Calculate the distribution of MAX consecutive zero-revenues years
consecutive_dist <- org_data %>%
  filter(YEAR <= 2020) %>% # I want to keep the Covid effect
  group_by(ORG_EIN) %>%
  summarize(max_consecutive_both_zeros = {
    both_zero = (F9_08_REV_TOT_TOT == 0 & F9_09_EXP_TOT_PROG == 0)
    runs <- rle(both_zero)
    # Identify the maximum length where the value is TRUE (i.e., it was a zero)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  count(max_consecutive_both_zeros)

print(consecutive_dist)

## -----------------------------------------------------------------------------
#Visualization
org_data %>%
  group_by(ORG_EIN) %>%
  summarize(max_streak = {
    both_zero = (F9_08_REV_TOT_TOT == 0 & F9_09_EXP_TOT_PROG == 0)
    runs <- rle(both_zero)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  ggplot(aes(x = factor(max_streak))) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Consecutive Zero-Expense & Zero-Revenue Years",
       x = "Longest Consecutive Streak of $0 Values",
       y = "Number of Organizations") +
  theme_minimal()

