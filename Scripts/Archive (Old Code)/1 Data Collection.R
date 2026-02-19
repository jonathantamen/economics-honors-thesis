## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
library(tidyverse)
library(readr)
library(dplyr)
setwd('/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Scripts')


## -----------------------------------------------------------------------------
#1. I need to get the folders.
all_folders = list.dirs(path = "../Raw Data", recursive = FALSE)
print(all_folders)


## -----------------------------------------------------------------------------
#2. I need to get the files, starting with just the first folder first as a test.
all_files_test = list.files(all_folders[1], recursive = FALSE, full.names = TRUE)
print(all_files_test)


## -----------------------------------------------------------------------------
#3. Creating loop to extract all files from all folders.
all_files_list = lapply(all_folders, list.files, recursive = FALSE, full.names = TRUE)
print(all_files_list)


## -----------------------------------------------------------------------------
list_of_dataframes_header = lapply(
  all_files_list[[1]], function(file_path) {
    
    #1. Adding year
    filename = basename(file_path)
    current_year = stringr::str_extract(file_path, "\\d{4}")
    
    #2. Selecting columns
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        ORG_EIN,
        F9_00_ORG_EIN,
        F9_00_ORG_ADDR_STATE,
        F9_00_ORG_ADDR_ZIP,
        F9_00_GRO_RCPT,
        # F9_00_EXEMPT_STAT_501C3_X, needs to be dropped
        F9_00_ORG_WEBSITE,
        F9_00_TYPE_ORG_CORP_X,
        F9_00_YEAR_FORMATION
      ) %>%
      mutate(Year = current_year) #adds new column
  }
)

# 2. Combine them into one data frame
header_data_combined = bind_rows(list_of_dataframes_header)

# 3. Check the result
print(tail(header_data_combined))


## -----------------------------------------------------------------------------
# 1. Process all files in the *second* folder list
list_of_dataframes_summary = lapply(
  all_files_list[[2]], function(file_path) {
    
    # Extract the year
    current_year <- stringr::str_extract(file_path, "\\d{4}")
    
    # Read, select, and add the new 'Year' column
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        ORG_EIN,
        F9_01_ACT_GVRN_ACT_MISSION,
        F9_01_ACT_GVRN_TERMIN_KONTR_X,
        F9_01_ACT_GVRN_NUM_VOTE_MEMB,
        F9_01_ACT_GVRN_NUM_VOTE_MEMB_IND
      ) %>%
      mutate(Year = current_year) 
  }
)

# 2. Combine them into one data frame
summary_data_combined <- bind_rows(list_of_dataframes_summary)

# 3. Check the result
print(tail(summary_data_combined))


## -----------------------------------------------------------------------------
# 1. Process all files in the *third* folder list
list_of_dataframes_mission = lapply(
  all_files_list[[3]], function(file_path) {
    
    # Extract the year
    current_year <- stringr::str_extract(file_path, "\\d{4}")
    
    # Read, select, and add the new 'Year' column
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        # F9_03_ORG_MISSION_PURPOSE, drop bc is redundant
        ORG_EIN,
        F9_03_PROG_NEW_X,
        F9_03_PROG_CHANGE_X
      ) %>%
      mutate(Year = current_year) 
  }
)

# 2. Combine them into one data frame
mission_data_combined <- bind_rows(list_of_dataframes_mission)

# 3. Check the result
print(tail(mission_data_combined))


## -----------------------------------------------------------------------------
# 1. Process all files in the *fifth* folder list
list_of_dataframes_governance <- lapply(
  all_files_list[[5]], function(file_path) {
    
    # Extract the first 4-digit number found in the file path
    current_year <- stringr::str_extract(file_path, "\\d{4}")
    
    # Read, select, and add the new 'Year' column
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        ORG_EIN,
        F9_06_GVRN_MEMB_STCKHLDR_X,
        F9_06_POLICY_JV_X
      ) %>%
      mutate(Year = current_year) 
  }
)

# 2. Combine them into one data frame
governance_data_combined <- bind_rows(list_of_dataframes_governance)

# 3. Check the result
print(tail(governance_data_combined))


## -----------------------------------------------------------------------------
# 1. Process all files in the *sixth* folder list
list_of_dataframes_trustkey <- lapply(
  all_files_list[[6]], function(file_path) {
    
    # Extract the first 4-digit number found in the file path
    current_year <- stringr::str_extract(file_path, "\\d{4}")
    
    # Read, select, and add the new 'Year' column
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        ORG_EIN,
        F9_07_COMP_DTK_COMP_ORG_TOT,
        F9_07_COMP_DTK_COMP_RLTD_TOT,
        F9_07_COMP_DTK_COMP_OTH_TOT
      ) %>%
      mutate(Year = current_year) 
  }
)

# 2. Combine them into one data frame
trustkey_data_combined <- bind_rows(list_of_dataframes_trustkey)

# 3. Check the result
print(tail(trustkey_data_combined))


## -----------------------------------------------------------------------------
# 1. Process all files in the *seventh* folder list
list_of_dataframes_revenue <- lapply(
  all_files_list[[7]], function(file_path) {
    
    # Extract the first 4-digit number found in the file path
    current_year <- stringr::str_extract(file_path, "\\d{4}")
    
    # Read, select, and add the new 'Year' column
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        ORG_EIN,
        F9_08_REV_CONTR_FED_CAMP,
        F9_08_REV_CONTR_MEMBSHIP_DUE,
        F9_08_REV_CONTR_FUNDR_EVNT,
        F9_08_REV_CONTR_RLTD_ORG,
        F9_08_REV_CONTR_GOVT_GRANT,
        F9_08_REV_CONTR_OTH,
        F9_08_REV_CONTR_TOT,
        F9_08_REV_PROG_TOT_TOT,
        F9_08_REV_OTH_INVEST_INCOME_TOT,
        F9_08_REV_OTH_INVEST_BOND_TOT,
        F9_08_REV_OTH_ROY_TOT,
        F9_08_REV_OTH_RENT_NET_TOT,
        F9_08_REV_OTH_SALE_GAIN_NET_TOT,
        F9_08_REV_OTH_FUNDR_NET_TOT,
        F9_08_REV_OTH_GAMING_NET_TOT,
        F9_08_REV_OTH_INV_NET_TOT,
        F9_08_REV_MISC_TOT_TOT,
        F9_08_REV_TOT_TOT
      ) %>%
      mutate(Year = current_year) 
  }
)

# 2. Combine them into one data frame
revenue_data_combined <- bind_rows(list_of_dataframes_revenue)

# 3. Check the result
print(tail(revenue_data_combined))


## -----------------------------------------------------------------------------
# 1. Process all files in the *eighth* folder list
list_of_dataframes_expenses <- lapply(
  all_files_list[[8]], function(file_path) {
    
    # Extract the first 4-digit number found in the file path
    current_year <- stringr::str_extract(file_path, "\\d{4}")
    
    # Read, select, and add the new 'Year' column
    read_csv(file_path, show_col_types = FALSE) %>%
      select(
        ORG_EIN,
        F9_09_EXP_GRANT_US_ORG_TOT,
        F9_09_EXP_GRANT_US_INDIV_TOT,
        F9_09_EXP_GRANT_FRGN_TOT,
        F9_09_EXP_TOT_TOT,
        F9_09_EXP_TOT_PROG,
        F9_09_EXP_TOT_MGMT,
        F9_09_EXP_TOT_FUNDR
      ) %>%
      mutate(Year = current_year) 
  }
)

# 2. Combine them into one data frame
expenses_data_combined <- bind_rows(list_of_dataframes_expenses)

# 3. Check the result
print(head(expenses_data_combined))


## -----------------------------------------------------------------------------
all_dataframe_list_list = list(
  header_data_combined,
  summary_data_combined,
  mission_data_combined,
  governance_data_combined,
  trustkey_data_combined,
  revenue_data_combined,
  expenses_data_combined
)

final_master_dataframe <- reduce(
  all_dataframe_list_list,
  full_join,
  by = c("ORG_EIN", "Year")
)



## -----------------------------------------------------------------------------
final_master_dataframe = final_master_dataframe |>
  distinct(ORG_EIN, Year, .keep_all = TRUE)


## -----------------------------------------------------------------------------
dir.create("Final_Data")
saveRDS(final_master_dataframe, "../Final_Data/final_master_data.rds")

