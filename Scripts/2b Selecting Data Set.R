#Imports
#Packages
library(dplyr)
library(tidyverse)

#Data frame
org_data = readRDS("../Final_Data/clean_master_data.rds")


#Set-up
# This code is for narrowing my data set. An extension to my data cleaning.

#--------------------------
#Part 1: Filtering Data Set
#To-Do:
# Schools/hospitals have inelastic demand for their services, so do not respond cyclically or counter-cyclically
# Organizations with multi-years of zero program expenses are not doing anything, no insights to gain.
# Column names should all be in lowercase, no abbreviations, with no spaces.

#Setup function to find zero-activity organizations
bad_org_ids <- org_data %>% #Thanks, AI.
  filter(YEAR < 2020) %>%
  arrange(ORG_EIN, YEAR) %>%
  group_by(ORG_EIN) %>%
  summarize(max_streak = {
    both_zero = (F9_08_REV_TOT_TOT == 0 & F9_09_EXP_TOT_PROG == 0)
    runs <- rle(both_zero)
    if(any(runs$values)) max(runs$lengths[runs$values]) else 0
  }) %>%
  filter(max_streak >= 3) %>%
  pull(ORG_EIN)


#Main filtering function
final_org_data = org_data %>%
  #----------------------
  #Step 1: Renaming columns
  rename(
    #Identifier variables
    organization_ein = "ORG_EIN",
    organization_name = "F9_00_ORG_NAME_L1",
    state = "F9_00_ORG_ADDR_STATE",
    year = "YEAR",
    industry = "activity_category",
    #Key variables
    total_program_expenses = "F9_09_EXP_TOT_PROG",
    total_revenue = "F9_08_REV_TOT_TOT",
    ) %>%
  #--------------------------------------------
  #Step 2: Dropping zero activity organizations
  filter(!(organization_ein %in% bad_org_ids)) %>%
  #----------------------------------
  #Step 3: Dropping hospitals/schools
  filter(org_type == "Neither")

#---------------------
#Step 3: Create Subsets



