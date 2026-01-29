# Imports
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
setwd('/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Scripts')

# ==============================================================================
# 1. Set-up
# This script takes a list of target variables (defined directly below) from the \n 
# US government Form 990 for non-profit organizations. This list can be updated
# ==============================================================================
target_variables <- c(
  # F990 • PART-00 • TABLE-00 • HEADER
  "F9_00_ORG_NAME_L1", #Name of organization
  "F9_00_ORG_ADDR_ZIP", #Zip code of organization HQ
  "F9_00_ORG_ADDR_CITY", #City of organization HQ
  "F9_00_ORG_ADDR_STATE", #State of organization HQ
  "F9_00_ORG_ADDR_CNTR", #Country of organization HQ
  #"F9_00_ORG_EIN", #Tax identification number. REDUNDANT.
  "F9_00_GRO_RCPT", #gross receipts. Total revenues (might be redundant)
  "F9_00_EXEMPT_STAT_501C3_X", #checkbox if 501(c) organization. Needs to be TRUE.
  #"F9_00_ORG_WEBSITE", #could be useful for discussion in end. 
  "F9_00_TYPE_ORG_CORP_X", #checkbox if corporation type org. needs to be TRUE.
  "F9_00_YEAR_FORMATION", #year org was founded. (Useful as control variable?)
  
  #F990 • PART-01 • TABLE-00 • SUMMARY. REMOVED.
  #"F9_01_ACT_GVRN_ACT_MISSION", #Describing the mission of org (string)
  #"F9_01_ACT_GVRN_TERMIN_KONTR_X", #Checkbox if organization discontinued activities. Prob needs to be FALSE.
  #"F9_01_ACT_GVRN_NUM_VOTE_MEMB", #Size of board of directors
  #"F9_01_ACT_GVRN_NUM_VOTE_MEMB_IND", #Independent voting members on board of directors (prob the same number as above for most orgs)
  
  # F990 • PART-03 • TABLE-00 • MISSION
  #"F9_03_ORG_MISSION_PURPOSE", #Again describing the organization’s mission.
  #"F9_03_PROG_NEW_X", #If org created any new programs this year. (Boolean). Additional Schedule O document would be to describe the details.
  #"F9_03_PROG_CHANGE_X", #If org ended any programs this year. (Boolean). Additional info in Schedule O.
  "F9_03_PROG_CODE", #North American Industry Classification System of non-profits main 3 activities. 
  
  # F990 • PART-06 • TABLE-00 • GOVERNANCE
  #"F9_06_GVRN_MEMB_STCKHLDR_X", #If org has stockholders
  #"F9_06_POLICY_JV_X", #If org invested in/with a for-profit business
  
  # F990 • PART-07 • TABLE-00 • DIR-TRUST-KEY
  "F9_07_COMP_DTK_COMP_ORG_TOT", #Total compensation of board of directors (salary)
  #"F9_07_COMP_DTK_COMP_RLTD_TOT", #Total compensation of board of directors (other sources)
  #"F9_07_COMP_DTK_COMP_OTH_TOT", #Total compensation of board of directors (non-cash)
  
  # F990 • PART-08 • TABLE-00 • REVENUE
  "F9_08_REV_CONTR_FED_CAMP", #Revenue from outside fundraising campaigns
  "F9_08_REV_CONTR_MEMBSHIP_DUE", #Revenue from membership dues
  "F9_08_REV_CONTR_FUNDR_EVNT", #Revenue from internal fundraising campaigns
  "F9_08_REV_CONTR_RLTD_ORG", #Revenue from related organizations?
  "F9_08_REV_CONTR_GOVT_GRANT", #Revenue from government contracts
  "F9_08_REV_CONTR_OTH", #Revenue from any other sources
  "F9_08_REV_CONTR_TOT", #Total revenues from contributions/donations
  "F9_08_REV_PROG_TOT_TOT", #Total revenues from program offerings/services
  "F9_08_REV_OTH_INVEST_INCOME_TOT", #Total revenues from investments
  "F9_08_REV_OTH_INVEST_BOND_TOT", #Total revenues from bonds
  "F9_08_REV_OTH_ROY_TOT", #Total revenues from royalties
  "F9_08_REV_OTH_RENT_NET_TOT", #Total income/loss from rents
  "F9_08_REV_OTH_SALE_GAIN_NET_TOT", #Total income/loss from sale of property
  "F9_08_REV_OTH_FUNDR_NET_TOT", #Total income/loss from fundraising
  "F9_08_REV_OTH_GAMING_NET_TOT", #Total income/loss from gambling fundraisers
  "F9_08_REV_OTH_INV_NET_TOT", #Total income/loss from sale of inventory
  "F9_08_REV_MISC_TOT_TOT", #Total income/loss from miscellaneous revenues
  "F9_08_REV_TOT_TOT", #Total revenues from all sources
  
  # F990 • PART-09 • TABLE-00 • EXPENSES
  "F9_09_EXP_GRANT_US_ORG_TOT", #Grants given to non-profits
  "F9_09_EXP_GRANT_US_INDIV_TOT", #Grants to individuals
  "F9_09_EXP_GRANT_FRGN_TOT", #Grants to foreign orgs/governments
  "F9_09_EXP_TOT_TOT", #Total of all expenses
  "F9_09_EXP_TOT_PROG", #Total of program expenses
  "F9_09_EXP_TOT_MGMT", #Total of management expenses
  "F9_09_EXP_TOT_FUNDR" #Total of fundraising expenses
)

# ==============================================================================
# 2. THE FUNCTION: Automates the search and extraction
# ==============================================================================

process_folder_dynamic <- function(folder_path, desired_vars) {
  
  # Get all files in this folder
  files <- list.files(folder_path, full.names = TRUE, recursive = FALSE)
  
  if(length(files) == 0) return(NULL)
  
  # --- INTELLIGENT CHECK ---
  # Read ONLY the header of the first file to see what columns live here.
  # We use n_max = 0 so it's instant.
  header_check <- read_csv(files[1], n_max = 0, show_col_types = FALSE)
  existing_cols <- names(header_check)
  
  # Find which of YOUR target variables exist in THIS folder
  vars_to_extract <- intersect(desired_vars, existing_cols)
  
  # Always ensure ORG_EIN is included for joining later
  if (!"ORG_EIN" %in% vars_to_extract) {
    # Check if ORG_EIN exists in the file at all
    if ("ORG_EIN" %in% existing_cols) {
      vars_to_extract <- c("ORG_EIN", vars_to_extract)
    } else {
      warning(paste("ORG_EIN not found in folder:", basename(folder_path)))
      return(NULL) # Skip this folder if we can't link it
    }
  }
  
  # If none of your target variables (besides EIN) are in this folder, skip it
  if (length(vars_to_extract) <= 1) {
    message(paste("Skipping folder", basename(folder_path), "- No target variables found."))
    return(NULL)
  }
  
  message(paste("Processing", basename(folder_path), "- extracting", length(vars_to_extract), "variables..."))
  
  # --- EXTRACT DATA ---
  folder_data <- lapply(files, function(file_path) {
    current_year <- str_extract(file_path, "\\d{4}")
    
    read_csv(file_path, col_select = all_of(vars_to_extract), show_col_types = FALSE) %>%
      mutate(Year = current_year)
  }) %>%
    bind_rows()
  
  return(folder_data)
}

# ==============================================================================
# 3. EXECUTION: Run the loop
# ==============================================================================

# Get your folders
all_folders <- list.dirs(path = "../Raw Data", recursive = FALSE)

# Run the function on every folder
# This returns a LIST of dataframes (one per valid folder)
list_of_dfs <- lapply(all_folders, process_folder_dynamic, desired_vars = target_variables)

# Remove NULL results (folders that didn't have any variables you wanted)
list_of_dfs <- Filter(Negate(is.null), list_of_dfs)

# ==============================================================================
# 4. MERGE: Combine everything
# ==============================================================================

message("Merging all dataframes...")

final_master_dataframe <- list_of_dfs %>%
  reduce(full_join, by = c("ORG_EIN", "Year")) %>%
  distinct(ORG_EIN, Year, .keep_all = TRUE)

# Check result
print(dim(final_master_dataframe))
print(names(final_master_dataframe))

# Save
dir.create("Final_Data") 

# Save as .rds file for easy coding
saveRDS(final_master_dataframe, "../Final_Data/final_master_data.rds")

# Save as .csv for easy sharing
write_csv(final_master_dataframe, "../Final_Data/final_master_data.csv")

# ==============================================================================
# 4. NEXT STEPS
#   a. Exploratory data analysis
#   b. Create new variables for financial ratios (overhead ratio, service dependence)
#   c. Data cleanup (possible nulls / NAs)
#   d. Import supplemental data (GDP, policy changes, etc)
# ==============================================================================
#
# 