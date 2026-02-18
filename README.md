# Non-profit Finances and Program Growth

**Author:** Jonathan Tamen ([GitHub here](https://github.com/JJoJonathan)) ([LinkedIn here](https://www.linkedin.com/in/jonathantamen/))

**Project:** Clark University Economics Honors Thesis 2026

**Research Question:** What 501(c)(3) non-profit organization financial practices are best correlated with expanding charity programs in good and bad times?

**This [[README.md]] is still in progress.
# Instructions

Follow these instructions in order, to replicate the research process.

## Set-up

Dependencies:

Raw Data Source: Jesse Lecy, (2023). NCCS IRS 990 Efile Data. <https://nccsgit.urban.org/nccs/datasets/efile/>

## Form 990 Data Import

This section outlines how to import the Form 990 data for types of non-profits (this project uses medium to large 501(c)(3) organizations that file F-990).

1. Be sure to download all the raw data from this site. ([990 E-FILER Data Catalog](https://nccsgit.urban.org/nccs/catalogs/catalog-efile-v2_1.html))
2. Run the entirety of script 1: `1 Data Collection.R` .

# Project Data Dictionary

Format: A data frame with 5 million observations across

# Next Steps?
#data_cleaning
- [x] Make samples all the same from the beginning. Else you don't know if the results are from changing sample or changing model. I.e. removing outliers all at the beginning.
- [x] Redo outliers elimination by double-checking distribution after each test. i.e. remove 1% top, or 2% or 5%.
- [ ] Look into entry/exit of organizations. 
- [x] Redo outliers elimination of organizations with zero expenses. First check distribution of organizations and occurrences of zero program expenses. i.e. just one year, can probably keep, but all years then probably remove. 
- [x] Double check fixed effects code with `feols()` functions. Rerun the regression for each industry. Fixed effect beta is an average. So individual industry is specific.
- [x] Double check fixed effects based on size of organization. Rerun the regression for each quintile size of organizations. 
- [] Try again with GDP per capita in place of real GDP, of each state. 
- [x] Don't need to include company/EIN fixed effects in all models. Just try without first. <!-- Maybe I can do once with, then repeat identical regression without? -->
#regressions
- [ ] Start testing explanatory variables (`) using [Regression 4]([[/Scripts/3a Preliminary Results.Rmd]]) from my preliminary results.
- [ ] Explore using an `interaction variable` with COVID-19 for financial decision-making changes of organizations. So, before/after COVID-19 to see if something changed in response to COVID-19 in how organizations respond to changing GDP.
- [ ] Explore using an `interaction variable` with federal policies for financial decision-making changes of organizations. So, funding to SNAP benefits, subsidized housing, research grants, interest rates. 
