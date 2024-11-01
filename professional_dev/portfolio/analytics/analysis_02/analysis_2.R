# For my second analysis, I will incorporate World Bank data into the UN data
# to generate more complex insights

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(purrr)
library(rworldxtra)
library(ggrepel)

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
wb_data_2016 <- wb_data_2016[ , !duplicated(colnames(wb_data_2016))]
wb_data_2016 <- wb_data_2016 %>%
  arrange(gdp)

# Subset the data based on region, merge with WB data
data(countriesHigh)
country_data <- countriesHigh@data %>%
  select(ISO_A3, continent, GEO3)

wb_data_2016 <- wb_data_2016 %>%
  left_join(country_data, by = c("ISO3_code" = "ISO_A3"))

# Drop the rows where a region was not identified, or GDP is unspecified
wb_data_2016 <- wb_data_2016 %>%
  filter(!is.na(continent) & continent != "")
wb_data_2016 <- wb_data_2016 %>%
  filter(!is.na(as.numeric(gsub(",", "", gdp))))
wb_data_2016$gdp <- as.numeric(as.character(wb_data_2016$gdp))

# Loop through each continent and display a plot for each
plot_path <- paste0(output_path, '01_analysis_2_gdp.pdf')
loc_names <- unique(wb_data_2016$continent)

# Start the PDF device
pdf(file = plot_path, width = 16, height = 10)

for (loc in loc_names){
  
  gg <- ggplot(wb_data_2016 %>% filter(continent == loc)) +
    aes(x = Location, y = gdp) +
    geom_point(size = 3) +
    geom_label_repel(aes(label = Location), size = 3, nudge_y = 0.5, max.overlaps = Inf) +
    theme_bw() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste0('National GDP per Capita (PPP)'),
         subtitle = paste0('Location: ', loc),
         x = NULL,
         y = "GDP/capita") + 
    theme(axis.text.x = element_blank())
  print(gg)
  
}

# Close the PDF device
dev.off()