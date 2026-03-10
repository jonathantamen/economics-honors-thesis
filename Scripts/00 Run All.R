# Master Script to run the data pipeline in sequence
# Installations
# install.package("fixest")
# install.package("modelsummary")
# install.packages("flextable")
# install.packages("tidyverse")

# Ensure we are in the correct working directory (the Scripts folder)
setwd(
    "/Users/jonat/Library/CloudStorage/OneDrive-ClarkUniversity/CU Courses/Economics Honors Thesis/Scripts"
) # Update as needed to your computer

print("Starting the data pipeline...")

# 1. Data Collection & Cleaning
print("Running Step 1: Data Collection & Cleaning...")
source("1 Data Collection (Better).R")
source("1 Activity Type Data Collection.R")
source("1 GDP Data Collection.R")
source("1b Cleaning Data.R")

# 2. Selecting & Exploring Data
print("Running Step 2: Selecting & Exploring Data...")
source("2a Selecting Data Set.R")
source("2b Exploration of Data Set.R")

# 3. Regressions & Analysis
print("Running Step 3: Regressions & Analysis...")
source("3a Preliminary Results.R")
source("3b Heterogeneity Analysis.R")
source("3c Covid Ineraction Regressions.R")

# 4. Explanatory Experiments
print("Running Step 4: Explanatory Variable Experiments...")
source("4 Explanatory Variable Experiments.R")

print("Data pipeline completed successfully!")
