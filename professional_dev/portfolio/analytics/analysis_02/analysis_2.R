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

wb_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/wb_occupational_09_04_2024.csv")

# Subsetting the WB data to only countries (removing regional rows)
iso3_codes <- un_data %>%
  pull(ISO3_code) %>%
  .[nzchar(.)] %>%
  unique()

wb_data <- wb_data %>%
  filter(Country.Code %in% iso3_codes)

# WB data is in long format, need to reformat to wide format and update some variables along the way
wb_data <- wb_data %>%
  mutate(Series.Name = na_if(Series.Name, "")) %>%
  distinct(Country.Code, Series.Name, .keep_all = TRUE) 

wb_data <- wb_data %>%
  pivot_longer(
    cols = starts_with("X"),       # Select columns starting with "X" (years columns)
    names_to = "Year",             # Name the new column for year
    values_to = "Value"            # Name the new column for values
  ) %>%
  # Correct the extraction of the year from column names
  mutate(
    Year = gsub("^X", "", Year),   # Remove leading 'X'
    Year = gsub("\\.\\.YR", "", Year),  # Remove '..YR'
    Year = substr(Year, 1, 4)      # Keep only the first 4 characters if the year is concatenated
  ) %>%
  # Ensure the Year column is numeric
  mutate(Year = as.numeric(Year)) %>%
  # Pivot wider based on Series.Name
  pivot_wider(
    names_from = Series.Name,   # Columns to become new column names
    values_from = Value         # Values to fill the new columns
  )

# Removing redundant country columns
wb_data <- wb_data %>%
  select(-Country.Name, -Series.Code)

# Look into the WB data columns to determine potential analyses
wb_cols <- data.frame(colnames(wb_data))

wb_data <- wb_data %>%
  select(
    Country.Code,
    Year,
    contains("Labor"),
    contains("Literacy"),
    contains("School"),
    contains("Unemployment"),
    contains("GDP per capita, PPP"),
    contains("GINI index")
  )

# I will be using the GDP per capita, PPP as a way to rank economic productivity of a given country
# Remove all rows where GDP is not reported (and rename variable for readability)
wb_data <- wb_data %>%
  filter(!is.na(`GDP per capita, PPP (constant 2011 international $)`))

wb_data <- wb_data %>%
  rename(gdp = `GDP per capita, PPP (constant 2011 international $)`)

# Next, I will merge together the WB data with the UN data to create one singular dataframe
un_data <- un_data %>%
  select(-SortOrder, -LocID, -Notes, -ISO2_code, -SDMX_code, -LocTypeID, -LocTypeName, -ParentID, -VarID,
         -Variant)

un_data <- un_data %>%
  filter(ISO3_code %in% iso3_codes)

wb_data <- left_join(un_data, wb_data, by = c("ISO3_code" = "Country.Code", "Time" = "Year"))

# WB Data only goes from 1990 to 2016, so removing all other rows
wb_data <- wb_data %>%
  mutate(Time = as.numeric(Time)) %>%
  filter(Time >= 1990 & Time <= 2016)

# 1.1 - Create a scatter plot comparing each country's GDP per capita
# Subsetting by year = 2016, and analyzing by region
wb_data_2016 <- filter(wb_data, Time == 2016)
wb_data_2016 <- wb_data_2016 %>%
  arrange(gdp)

# NEXT: subset the data based on region. Find an R package that can map ISO3 codes to region

ggplot(wb_data_2016, aes(x = ISO3_code, y = gdp)) +
  geom_point(size = 3) +  # Add points
  geom_text(aes(label = ISO3_code), vjust = -1) +  # Add labels
  labs(title = "GDP per Capita (PPP) by ISO3 Code",
       x = "ISO3 Code",
       y = "GDP per Capita (PPP, constant 2011 $)") +
  theme_minimal()
