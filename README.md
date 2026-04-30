# Non-profit Finances and Program Growth

**Author:** Jonathan Tamen ([GitHub here](https://github.com/JJoJonathan)) ([LinkedIn here](https://www.linkedin.com/in/jonathantamen/))

**Project:** Clark University Economics Honors Thesis 2026

**Research Question:** What revenue sources are most correlated with helping non-profits do more when the economy is doing worse?

# Instructions

Follow these instructions in order, to replicate the research process.

## Set-up

Dependencies:

- tidyverse: Used for data cleaning, manipulation, and visualization (includes dplyr, ggplot2, readr, tidyr, and stringr).
- fixest: Used for high-dimensional fixed-effects linear regressions.
- modelsummary: Used to generate professional regression tables.
- flextable: Used to format and export tables directly into Word documents.
- officer: Required for the programmatic generation of Word documents.
- scales: Used for advanced axis labeling and number formatting in graphics.
- knitr: Used for dynamic report generation and setup.

To make it easier, run this code in R to download them all before following the next steps.

```r
install.packages(c("tidyverse", "fixest", "modelsummary", "flextable", "officer", "scales", "knitr"))
```

## Raw Data Source

```
Jesse Lecy, (2023). NCCS IRS 990 Efile Data. <https://nccsgit.urban.org/nccs/datasets/efile/>
```

## Form 990 Data Collection

This section outlines how to import the Form 990 data for types of non-profits (this project uses medium to large 501(c)(3) organizations that file F-990).

1. Be sure to download all the raw data from the raw data source. Linked directly from [990 E-FILER Data Catalog](https://nccsgit.urban.org/nccs/catalogs/catalog-efile-v2_1.html). Save the zip files to a folder '/Raw_Data'. My code will automatically look for the zip files and extract them.
2. Download my code files, the '/Scripts' folder. You will also need to setup a '/Final_Data' and '/Outputs' folders.

## Coded Methodology

3. From there, I made it easy to replicate my entire research process. You can just run the entirety of the '00 - Run All.R' script. It will call every other script in order. It will output all the regressions as a table (and some as images) to '/Outputs' folder.

# Project Data Dictionary

My data uses 2.88 million observations across 300,000 non-profit organizations from 2009 - 2023. Specifically, organizations that have applied for and received 501(c)(3) tax-exempt status from the IRS, have gross receipts greater than $250,000, and are headquarted in the United States.

# Next Steps?

- [x] Make samples all the same from the beginning. Else you don't know if the results are from changing sample or changing model. I.e. removing outliers all at the beginning.
- [x] Redo outliers elimination by double-checking distribution after each test. i.e. remove 1% top, or 2% or 5%.
- [x] Look into entry/exit of organizations.
- [x] Redo outliers elimination of organizations with zero expenses. First check distribution of organizations and occurrences of zero program expenses. i.e. just one year, can probably keep, but all years then probably remove.
- [x] Double check fixed effects code with `feols()` functions. Rerun the regression for each industry. Fixed effect beta is an average. So individual industry is specific.
- [x] Double check fixed effects based on size of organization. Rerun the regression for each quintile size of organizations.
- [] ~~Try again with GDP per capita in place of real GDP, of each state. ~~
- [x] Don't need to include company/EIN fixed effects in all models. Just try without first.

- [] ~~Include sum of revenues, expenses divided by type across all the organizations. (i.e. what is the split between donations, fundraising, government revenues and the split between program expenses, staff, assets expenses).~~
- [x] Create graphic of change in State GDP over time.
# regressions
- [x] Start testing explanatory variables from my preliminary results.
- [x] Explore using an `interaction variable` with COVID-19 for financial decision-making changes of organizations. So, before/after COVID-19 to see if something changed in response to COVID-19 in how organizations respond to changing GDP.
# analysis
- [x] Finish interpreting all results from all regressions.
- [x] Analyze results in terms of statistical significance, economic logic, and topic relevance.
- [x] Produce “Preliminary Results” Document for review.
# final_paper
- [x] Format all tables to match Chicago format
- [x] Insert links to tables/figures, instead of textual references
- [x] Create bibliography and update footnotes to correct repeats.
