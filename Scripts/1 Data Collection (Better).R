# ==============================================================================
# STEP 1a: Dynamic Data Collection from IRS Form 990
# Economics Honors Thesis Data Pipeline
# ==============================================================================
# This script extracts selected financial and organizational variables from
# raw CSV files containing annual IRS Form 990 filings. Since IRS datasets
# are extremely large, this script is optimized to scan and extract only
# the relevant columns, saving memory and computational time.
# ==============================================================================

# Core Libraries
library(tidyverse) # Collection of packages for data science (includes ggplot2, dplyr, tidyr, readr)
library(readr)     # Fast and friendly way to read rectangular data (like csv)
library(dplyr)     # Grammar for data manipulation
library(stringr)   # Consistent, simple wrappers for common string operations

# ==============================================================================
# 1. Target Variables Setup
# Here, we specify exactly which variables (columns) we want to extract from the
# IRS Form 990 data. These variables map to specific sections of Form 990.
# ==============================================================================
target_variables <- c(
  # --- F990 • PART-00 • HEADER & IDENTIFICATION ---
  "F9_00_ORG_NAME_L1",         # Legal Name of the non-profit organization
  "F9_00_ORG_ADDR_ZIP",        # ZIP/Postal code of headquarters
  "F9_00_ORG_ADDR_CITY",       # City of headquarters
  "F9_00_ORG_ADDR_STATE",      # State of headquarters (crucial for linking with state-level GDP)
  "F9_00_ORG_ADDR_CNTR",       # Country of headquarters (we filter to "US" later)
  "F9_00_GRO_RCPT",            # Gross receipts (total income before expenses/deductions)
  "F9_00_EXEMPT_STAT_501C3_X", # Logical checkbox: TRUE if registered as a 501(c)(3) tax-exempt organization
  "F9_00_TYPE_ORG_CORP_X",     # Logical checkbox: TRUE if the organization is structured as a corporation
  "F9_00_YEAR_FORMATION",      # Year the organization was officially founded (useful control variable)

  # --- F990 • PART-03 • NAICS CLASSIFICATION ---
  "F9_03_PROG_CODE",           # North American Industry Classification System (NAICS) code for main activities

  # --- F990 • PART-07 • GOVERNANCE & COMPENSATION ---
  "F9_07_COMP_DTK_COMP_ORG_TOT", # Total salary/compensation paid to directors, trustees, and key employees

  # --- F990 • PART-08 • REVENUE SOURCES (Supply Side) ---
  "F9_08_REV_CONTR_FED_CAMP",       # Revenue from federated fundraising campaigns
  "F9_08_REV_CONTR_MEMBSHIP_DUE",    # Revenue from membership dues
  "F9_08_REV_CONTR_FUNDR_EVNT",      # Net revenue from internal fundraising events
  "F9_08_REV_CONTR_RLTD_ORG",        # Revenue from related organizations
  "F9_08_REV_CONTR_GOVT_GRANT",      # Revenue from government contracts and grants
  "F9_08_REV_CONTR_OTH",             # Revenue from other contributions/donations
  "F9_08_REV_CONTR_TOT",             # Total revenues from all contributions (sum of above)
  "F9_08_REV_PROG_TOT_TOT",          # Total revenue generated from program services/fees
  "F9_08_REV_OTH_INVEST_INCOME_TOT",  # Net income/loss from investment activities
  "F9_08_REV_OTH_INVEST_BOND_TOT",    # Net income/loss from government and municipal bonds
  "F9_08_REV_OTH_ROY_TOT",            # Net revenue from royalties
  "F9_08_REV_OTH_RENT_NET_TOT",       # Net income/loss from rental properties
  "F9_08_REV_OTH_SALE_GAIN_NET_TOT",  # Net gain/loss from sales of assets/property
  "F9_08_REV_OTH_FUNDR_NET_TOT",      # Net income/loss from professional fundraising activities
  "F9_08_REV_OTH_GAMING_NET_TOT",     # Net income/loss from gaming/gambling activities
  "F9_08_REV_OTH_INV_NET_TOT",        # Net income/loss from inventory sales
  "F9_08_REV_MISC_TOT_TOT",           # Miscellaneous other revenue
  "F9_08_REV_TOT_TOT",                # Total revenue from all combined sources

  # --- F990 • PART-09 • FUNCTIONAL EXPENSES (Demand Side) ---
  "F9_09_EXP_GRANT_US_ORG_TOT",      # Grants/assistance given to other US organizations
  "F9_09_EXP_GRANT_US_INDIV_TOT",    # Grants/assistance given to US individuals (e.g. scholarships)
  "F9_09_EXP_GRANT_FRGN_TOT",        # Grants/assistance given to foreign organizations/individuals
  "F9_09_EXP_TOT_TOT",               # Total expenditures (sum of program + admin + fundraising)
  "F9_09_EXP_TOT_PROG",              # Expenditures directly spent on programs (our key demand proxy)
  "F9_09_EXP_TOT_MGMT",              # Expenditures spent on management and administration
  "F9_09_EXP_TOT_FUNDR"              # Expenditures spent on fundraising activities
)

# ==============================================================================
# 2. THE EXTRACTION FUNCTION
# This function dynamically processes a directory, checks which target variables
# exist in the files, and reads only those specific columns to optimize memory.
# ==============================================================================
process_folder_dynamic <- function(folder_path) {
  # Retrieve paths to all files in the target folder
  files <- list.files(folder_path, full.names = TRUE, recursive = FALSE)

  # If the folder is empty, return NULL and skip
  if (length(files) == 0) {
    return(NULL)
  }

  # --- HEADER CHECK ---
  header_check <- read_csv(files[1], n_max = 0, show_col_types = FALSE)
  existing_cols <- names(header_check)

  # Always ensure 'ORG_EIN' (Employer Identification Number) is included.
  # This is the unique key we need to link files together.
  if (!"ORG_EIN" %in% existing_cols) {
    warning(paste("ORG_EIN not found in folder:", basename(folder_path)))
    return(NULL)
  }

  message(paste("Processing folder", basename(folder_path), "- extracting all", length(existing_cols), "variables..."))

  # --- MULTI-FILE READING LOOP ---
  # Read each file in the folder, extracting all columns, and record the tax year
  folder_data <- lapply(files, function(file_path) {
    # Extract the 4-digit year from the filename (e.g. "2020" from "filing_2020.csv")
    current_year <- str_extract(file_path, "\\d{4}")

    # Read all columns
    read_csv(file_path, show_col_types = FALSE) |>
      mutate(Year = current_year) # Attach the year column
  }) |>
    bind_rows() # Combine list of annual data frames into a single data frame

  return(folder_data)
}

# ==============================================================================
# 3. PIPELINE EXECUTION
# ==============================================================================

# 3a. Retrieve paths of all subfolders inside the raw_data directory
# Each folder represents a different section of Form 990 or a distinct filing category.
# We check both "raw_data" and "Raw_Data" to be robust.
raw_data_dir <- "../raw_data"
if (!dir.exists(raw_data_dir)) {
  raw_data_dir <- "../Raw_Data"
}

all_folders <- list.dirs(path = raw_data_dir, recursive = FALSE)

if (length(all_folders) > 0) {
  # --- Folder-based mode ---
  message("Found subdirectories in raw_data. Processing folder-based structure...")
  # Apply the processing function to each raw folder
  list_of_dfs <- lapply(all_folders, process_folder_dynamic)
} else {
  # --- File-based mode (for reduced flat datasets) ---
  message("No subdirectories found in raw_data. Processing CSV files directly...")
  all_files <- list.files(path = raw_data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(all_files) == 0) {
    stop("No subdirectories or CSV files found in the raw data directory.")
  }
  
  # Process each file individually
  process_file_dynamic <- function(file_path) {
    # Read header to find available columns
    header_check <- read_csv(file_path, n_max = 0, show_col_types = FALSE)
    existing_cols <- names(header_check)
    
    # Ensure ORG_EIN is included
    if (!"ORG_EIN" %in% existing_cols) {
      warning(paste("ORG_EIN not found in file:", basename(file_path)))
      return(NULL)
    }
    
    message(paste("Processing file", basename(file_path), "- extracting all", length(existing_cols), "variables..."))
    
    # Extract year from filename, or default to TAX_YEAR column, or fallback to 2024
    current_year <- str_extract(file_path, "\\d{4}")
    if (is.na(current_year)) {
      if ("TAX_YEAR" %in% existing_cols) {
        temp_df <- read_csv(file_path, n_max = 1, col_select = all_of("TAX_YEAR"), show_col_types = FALSE)
        if (nrow(temp_df) > 0) {
          current_year <- as.character(temp_df$TAX_YEAR[1])
        }
      }
      if (is.na(current_year)) {
        current_year <- "2024"
      }
    }
    
    # Read all data
    read_csv(file_path, show_col_types = FALSE) |>
      mutate(Year = current_year)
  }
  
  list_of_dfs <- lapply(all_files, process_file_dynamic)
}

# 3c. Remove NULL elements (folders/files that had no matching data or headers)
list_of_dfs <- Filter(Negate(is.null), list_of_dfs)

# ==============================================================================
# 4. DATA MERGING & EXPORT
# ==============================================================================
message("Merging all dataframes...")

# 4a. Sequentially perform a full outer join on all data frames using all common variables.
# This aligns all variables by organization and year without duplicate columns.
if (length(list_of_dfs) == 0) {
  stop("No data frames to merge.")
}

final_master_dataframe <- list_of_dfs[[1]]
if (length(list_of_dfs) > 1) {
  for (i in 2:length(list_of_dfs)) {
    common_cols <- intersect(names(final_master_dataframe), names(list_of_dfs[[i]]))
    
    # Coerce all common columns to character in both dataframes to prevent type conflicts during join
    final_master_dataframe <- final_master_dataframe |>
      mutate(across(all_of(common_cols), as.character))
    list_of_dfs[[i]] <- list_of_dfs[[i]] |>
      mutate(across(all_of(common_cols), as.character))
      
    message(paste("Joining next dataset on common columns:", paste(common_cols, collapse = ", ")))
    final_master_dataframe <- full_join(final_master_dataframe, list_of_dfs[[i]], by = common_cols)
  }
}

final_master_dataframe <- final_master_dataframe |>
  distinct(ORG_EIN, Year, .keep_all = TRUE)

# Print structural diagnostics
print("Merged dataset dimensions (Rows x Columns):")
print(dim(final_master_dataframe))
print("Available variable names:")
print(names(final_master_dataframe))

# Create the output directory if it does not exist
if (!dir.exists("../Final_Data")) {
  dir.create("../Final_Data")
}

# 4b. Save as .rds file
# RDS is a native R file format. It preserves R data types (e.g. factors, dates)
# and is compressed, making it much faster to read/write in subsequent scripts.
saveRDS(final_master_dataframe, "../Final_Data/final_master_data.rds")

# 4c. Save as .csv file for external use (sharing, viewing in Excel)
write_csv(final_master_dataframe, "../Final_Data/final_master_data.csv")
