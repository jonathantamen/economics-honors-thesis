# ==============================================================================
# MASTER RUNNER: Data Pipeline Sequence (Data Lookup Edition)
# Economics Honors Thesis Data Pipeline
# ==============================================================================
# This script orchestrates the data collection, industry mapping, and cleanup
# sequence, creating a compiled master dataset of organizations that can be 
# filtered and refined in R.
# ==============================================================================

# Ensure we are in the correct working directory (the Scripts folder)
setwd(
    "/Users/jonat/Code/economics-honors-thesis/Scripts"
)

message(">>> Starting the economics honors thesis data lookup pipeline...")

# ==============================================================================
# Step 1: Data Collection & Cleaning
# ==============================================================================
message(">>> Running Step 1: Data Collection & Cleaning...")

# 1a. Core Form 990 Data Collection (Better/dynamic version)
source("1 Data Collection (Better).R")

# 1b. IRS Activity Codes translation to NTEE industry categories
# Note: Requires eo1.csv, eo2.csv, eo3.csv in Raw_Data/IRS Activity Codes
if (file.exists("../Final_Data/activity_data.rds")) {
  message(">>> Skipping 1 Activity Type Data Collection.R (using existing processed RDS)")
} else if (dir.exists("../Raw_Data/IRS Activity Codes") || dir.exists("../raw_data/IRS Activity Codes")) {
  source("1 Activity Type Data Collection.R")
} else {
  message(">>> Warning: Raw IRS Activity Codes files not found. Activity mapping skipped.")
}

# 1c. State GDP Data Collection & processing from Bureau of Economic Analysis (BEA)
# Note: Requires SAGDP1__ALL_AREAS_1997_2024.csv in Raw_Data/SAGDP
if (file.exists("../Final_Data/gdp_data.rds")) {
  message(">>> Skipping 1 GDP Data Collection.R (using existing processed RDS)")
} else if (dir.exists("../Raw_Data/SAGDP") || dir.exists("../raw_data/SAGDP")) {
  source("1 GDP Data Collection.R")
} else {
  message(">>> Warning: Raw BEA GDP files not found. GDP processing skipped.")
}

# 1d. Merge core, activity, and GDP data into a clean master file
if (file.exists("../Final_Data/final_master_data.rds")) {
  source("1b Cleaning Data.R")
} else {
  message(">>> Error: Master collected data not found. Merging skipped.")
}

# ==============================================================================
# Step 2: Selecting & Refining Data Set
# ==============================================================================
message(">>> Running Step 2: Selecting & Refining Data Set...")

# 2a. Filter master dataset (handling singletons, dormant organizations, and creating base variables)
if (file.exists("../Final_Data/clean_master_data.rds") && 
    "activity_category" %in% names(readRDS("../Final_Data/clean_master_data.rds"))) {
  source("2a Selecting Data Set.R")
} else {
  message(">>> Warning: clean_master_data.rds or industry categories not found. Skipping Step 2a.")
}

message(">>> Data pipeline completed successfully! Compiled datasets are saved in '../Final_Data'.")
