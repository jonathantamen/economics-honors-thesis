# Non-profit Finances and Program Growth

**Author:** Jonathan Tamen ([Github here](https://github.com/JJoJonathan)) ([Linkedin here](https://www.linkedin.com/in/jonathantamen/))

**Project:** Clark University Economics Honors Thesis 2026

**Research Question:** What 501(c)(3) non-profit organization financial practices are best correlated with expanding charity programs in good and bad times?

This README is still in progress.

# Instructions

Follow these instructions in order, to replicate the research process.

## Set-up

Dependencies:

Raw Data Source: Jesse Lecy, (2023). NCCS IRS 990 Efile Data. <https://nccsgit.urban.org/nccs/datasets/efile/>

## Form 990 Data Import

This section outlines how to import the Form 990 data for types of non-profits (this project uses medium to large 501(c)(3) organizations that file F-990.

1.  Be sure to download all the raw data from this site. ([990 EFILER Data Catalog](https://nccsgit.urban.org/nccs/catalogs/catalog-efile-v2_1.html))
2.  Run the entirety of script 1: `1 Data Collection.R` . You will have to change `line 6: setwd()` based on where you save the raw data and your code.

# Project Data Dictionary

Format: A data frame with 5 million observations across
