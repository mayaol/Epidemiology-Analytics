# For my first analysis, I will investigate the UN Population dataset and
# create several visualizations

install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("scales")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(purrr)

output_path <- "C:/Users/mayaol/professional_dev/portfolio/analytics/"

# 1. Investigate dataset
un_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/un_pop_08_15_2024.csv")
un_cols <- data.frame(colnames(un_data))

# For this analysis, no subnational data will be used
un_data <- un_data %>%
  filter(LocTypeName == "Country/Area")

# 1.1 Find out which countries had/will have the highest population in each given 
#     year: 1950, 1975, 2000, 2025, and 2050
# Using a loop function to filter out the UN data by each year
pop_years <- un_data %>%
  select('Location','ISO3_code','Time','TPopulation1Jan')
years <- c(1950, 1975, 2000, 2025, 2050)

pop_data <- map(years, function(year) {
  pop_years %>%
    filter(Time == year)
})
names(pop_data) <- paste0("pop_", years)

pop_1950 <- pop_data$pop_1950
pop_1975 <- pop_data$pop_1975
pop_2000 <- pop_data$pop_2000
pop_2025 <- pop_data$pop_2025
pop_2050 <- pop_data$pop_2050

# Next, creating a function to (1) sort each dataframe in descending order based
# on the population, and (2) conserve only the first row, that is, the country
# with the highest population that year.
pop_list <- list(
  pop_1950 = pop_1950,
  pop_1975 = pop_1975,
  pop_2000 = pop_2000,
  pop_2025 = pop_2025,
  pop_2050 = pop_2050
)

sort_function <- function(df) {
  df %>%
    arrange(desc(TPopulation1Jan)) %>%
    slice(1)
}

un_pop_final <- map(pop_list, sort_function)
un_pop_final <- bind_rows(un_pop_final, .id = "source")

# Exporting to folder
write_csv(un_pop_final, "C:/Users/mayaol/professional_dev/portfolio/analytics/01_analysis_1_highest_pop.csv")

# 1.2 Find out which country had the greatest change in population from 1950 to 2000
# First, filtering to preserve only the 1950 and 2000 year rows
pop_1950_2000 <- un_data %>%
  filter(Time %in% c(1950, 2000)) %>%
  select('Location','ISO3_code','Time','TPopulation1Jan')

# Re-formatting the dataframe and creating a new column to reflect population diff
pop_1950_2000 <- pop_1950_2000 %>%
  pivot_wider(
    names_from = Time,
    values_from = TPopulation1Jan,
    names_prefix = "TPopulation_"
  )

pop_1950_2000 <- pop_1950_2000 %>%
  mutate(PopDiff = TPopulation_2000 - TPopulation_1950) %>%
  select(Location, ISO3_code, PopDiff)

# Sorting in descending order and conserving the first row only
pop_1950_2000 <- pop_1950_2000 %>%
arrange(desc(PopDiff)) %>%
  slice(1)

# Exporting to folder
write_csv(pop_1950_2000, "C:/Users/mayaol/professional_dev/portfolio/analytics/02_analysis_1_greatest_diff_1950_2000.csv")

# 1.3 Calculate each country's Infant Mortality Rate (IMR) and check that it 
#     matches what is reported
un_imr <- un_data %>%
  select('Location','ISO3_code','Time','TPopulation1July','InfantDeaths','Births','IMR')

# IMR is the ratio of the number of infant deaths compared to live births in a 
# given place and time, times 1000. In this dataset, the births are reported as births per
# 1,000 population, and infant deaths are reported in the thousands
un_imr <- un_imr %>%
  mutate(Births = Births * 1000) %>%
  mutate(InfantDeaths = InfantDeaths * 1000)

# Calculate the IMR, and compare it to the reported value
un_imr <- un_imr %>%
  mutate(manual_IMR = (InfantDeaths / Births) * 1000)

# Using this method, the IMR I calculated is very similar to the one reported by
# UN, with some slight discrepancies that may be due to some data/calculation differences
# Exporting to folder
write_csv(un_imr, "C:/Users/mayaol/professional_dev/portfolio/analytics/03_analysis_1_calculating_IMR.csv")

# 2. World map of population
# More specifically, I will do a world map of population by country for the year 2015
# Keeping only necessary columns, filtering by the year 2015
un_data_1 <- un_data %>%
  select('Location','ISO3_code','Time','PopDensity') %>%
  filter(Time == 2015)

# Setting up world map data
global <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
un_data_1 <- left_join(global, un_data_1, by = c("adm0_a3" = "ISO3_code"))

# Creating world map plot here
un_pop_map <- ggplot(data = un_data_1) +
  geom_sf(aes(fill = PopDensity), color = "black") +
  scale_fill_viridis_c(name = "Population (persons/km^2)", 
                       labels = label_comma(),
                       option = "C",
                       direction = -1,
                       limits = c(0, 500),
                       breaks = seq(0, 500, by = 50),
                       guide = guide_colorbar(
                         barheight = unit(0.5, "cm"),
                         barwidth = unit(10, "cm"))
  ) +
  theme_minimal() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.key.size = unit(1.5, "cm")
  ) + 
  labs(title = "World Map with Population Density, 2015",
       subtitle = "Data from United Nations") +
  theme(legend.position = "bottom")

# Exporting to folder
ggsave(filename = "C:/Users/mayaol/professional_dev/portfolio/analytics/04_analysis_1_worldmap.jpg", 
       plot = un_pop_map, width = 14, height = 7)