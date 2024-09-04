# For my second analysis, I will incorporate World Bank data into the UN data
# to generate more complex insights

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(purrr)

output_path <- "C:/Users/mayaol/professional_dev/portfolio/analytics/analysis_02/"

# 1. Investigate dataset
un_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/un_pop_08_15_2024.csv")
un_cols <- data.frame(colnames(un_data))

wb_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/wb_occupational_final_08_29_2024.csv")

# Subsetting the WB data to only countries (removing regional rows)
iso3_codes <- un_data %>%
  pull(ISO3_code) %>%
  unique()

wb_data <- wb_data %>%
  filter(Country.Code %in% iso3_codes)

# WB data is in long format, need to reformat to wide format and update some variables along the way
wb_data <- wb_data %>%
  mutate(Series.Name = na_if(Series.Name, "")) %>%
  select(Country.Name, Country.Code, Series.Name, X2015..YR2015.) %>% 
  distinct(Country.Name, Series.Name, .keep_all = TRUE) 

wb_data <- wb_data %>%
  pivot_wider(
    names_from = Series.Name,   # This will become the new column names
    values_from = X2015..YR2015. # This is the data for the new columns
  )

wb_data <- wb_data %>%
  mutate(across(everything(), ~ na_if(.x, "..")))

# Look into the WB data columns to determine potential analyses
wb_cols <- data.frame(colnames(wb_data))