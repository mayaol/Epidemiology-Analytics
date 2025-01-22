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
library(countrycode)

output_path <- "C:/Users/mayaol/professional_dev/portfolio/analytics/analysis_02/"

# 1. Investigate dataset
un_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/un_pop_08_15_2024.csv")
un_cols <- data.frame(colnames(un_data))

wb_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/wb_occupational_09_04_2024.csv")

# Subsetting the WB and UN data to only countries (removing regional rows)
un_data <- un_data %>%
  filter(ISO3_code != "")

wb_data <- wb_data %>%
  filter(Country.Code %in% un_data$ISO3_code)

# WB data is in long format, need to reformat to wide format and update some variables along the way
wb_data <- wb_data %>%
  mutate(Series.Name = na_if(Series.Name, "")) %>%
  distinct(Country.Code, Series.Name, .keep_all = TRUE) 

# Year columns include extra letters, updating this here
colnames(wb_data) <- sapply(colnames(wb_data), function(colname) {
  if (grepl("^X\\d{4}\\.\\.YR\\d{4}\\.$", colname)) {  # Match year columns
    # Extract the year (4 digits)
    return(gsub(".*(\\d{4}).*", "\\1", colname))
  } else {
    # Leave non-year columns as they are
    return(colname)
  }
})

wb_data <- wb_data %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  # Select columns where the name is exactly a 4-digit year (YYYY)
    names_to = "Year",            # Name the new column for year
    values_to = "Value"           # Name the new column for values
  ) %>%
  # Ensure only one row per combination of Country.Name, Country.Code, Year, and Series.Name
  group_by(Country.Name, Country.Code, Year, Series.Name) %>%
  summarize(Value = first(Value), .groups = "drop") %>%
  pivot_wider(
    names_from = Series.Name,     # Use Series.Name to create new columns
    values_from = Value,          # Fill the new columns with the corresponding Value
    values_fn = list(Value = first)  # In case of multiple values, take the first one (or change as needed)
  )

# Removing redundant country columns, updating NA values
wb_data <- wb_data %>%
  mutate(across(everything(), ~na_if(., '..')))

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

# Next, I will merge together the WB data with the UN data to create one singular dataframe
# WB Data only goes from 1990 to 2016, so removing all other years from UN data
un_data <- un_data %>%
  mutate(Time = as.numeric(Time)) %>%
  filter(Time >= 1990 & Time <= 2016)

un_data <- un_data %>%
  select(-SortOrder, -LocID, -Notes, -ISO2_code, -SDMX_code, -LocTypeID, -LocTypeName, -ParentID, -VarID,
         -Variant)

un_data <- un_data %>%
  mutate(Time = as.numeric(Time))
wb_data <- wb_data %>%
  mutate(Year = as.numeric(Year))

wb_un_data <- left_join(un_data, wb_data, by = c("ISO3_code" = "Country.Code", "Time" = "Year"))

# Removing any columns where more than 50% of the rows are NA, for readability
wb_un_data <- wb_un_data %>%
  select(where(~ mean(is.na(.)) <= 0.5))

# 1.1 - Create a scatter plot comparing each country's GDP per capita
# Subsetting by year = 2016, and analyzing by region

# I will be using the GDP per capita, PPP as a way to rank economic productivity of a given country
# Remove all rows where GDP is not reported (and rename variable for readability)
wb_data_gdp <- wb_un_data %>%
  filter(!is.na(`GDP per capita, PPP (constant 2011 international $)`))

wb_data_gdp <- wb_data_gdp %>%
  rename(gdp = `GDP per capita, PPP (constant 2011 international $)`)

wb_data_gdp_2016 <- filter(wb_data_gdp, Time == 2016)
wb_data_gdp_2016 <- wb_data_gdp_2016[ , !duplicated(colnames(wb_data_gdp_2016))]
wb_data_gdp_2016 <- wb_data_gdp_2016 %>%
  arrange(gdp)

# Subset the data based on region, merge with WB data
data(countriesHigh)
country_data <- countriesHigh@data %>%
  select(ISO_A3, continent, GEO3)

wb_data_gdp_2016 <- wb_data_gdp_2016 %>%
  left_join(country_data, by = c("ISO3_code" = "ISO_A3"))

# Drop the rows where a region was not identified, or GDP is unspecified
wb_data_gdp_2016 <- wb_data_gdp_2016 %>%
  filter(!is.na(continent) & continent != "")
wb_data_gdp_2016 <- wb_data_gdp_2016 %>%
  filter(!is.na(as.numeric(gsub(",", "", gdp))))
wb_data_gdp_2016$gdp <- as.numeric(as.character(wb_data_gdp_2016$gdp))

# Loop through each continent and display a plot for each
plot_path <- paste0(output_path, '01_analysis_2_gdp.pdf')
loc_names <- unique(wb_data_gdp_2016$continent)

# Start the PDF device
pdf(file = plot_path, width = 16, height = 10)

for (loc in loc_names){
  
  gg <- ggplot(wb_data_gdp_2016 %>% filter(continent == loc)) +
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

# 1.2 - Create a scatter plot comparing each country's infant mortality rate (IMR)
# with primary school enrollment rate
# Subsetting by year = 2016