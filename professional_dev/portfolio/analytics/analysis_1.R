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

output_path <- "C:/Users/mayaol/professional_dev/portfolio/analytics/"

# 1. Investigate dataset
un_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/un_pop_08_15_2024.csv")
un_cols <- data.frame(colnames(un_data))

# For this analysis, no subnational data will be used
un_data <- un_data %>%
  filter(LocTypeName == "Country/Area")

# 1.1 Find out which countries had/will have the highest population in each given 
#     year: 1950, 1975, 2000, 2025, and 2050

# 1.3 Find out which country had the greatest change in population from 1950 to 2000

# 1.2 Calculate each country's Infant Mortality Rate (IMR) and check that it 
#     matches what is reported

# 2. World map of population
# More specifically, I will do a world map of population by country for the year 2015
# Keeping only necessary columns, filtering by the year 2015
un_data_1 <- un_data %>%
  select('Location','ISO3_code','Time','TPopulation1Jan') %>%
  filter(Time == 2015)

# Setting up world map data
global <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
un_data_1 <- left_join(global, un_data_1, by = c("adm0_a3" = "ISO3_code"))

# For readability, converting the population to millions
un_data_1 <- un_data_1 %>%
  mutate(TPopulation1Jan = TPopulation1Jan / 1000)

# Creating world map plot here
un_pop_map <- ggplot(data = un_data_1) +
  geom_sf(aes(fill = TPopulation1Jan), color = "black") +
  scale_fill_viridis_c(name = "Population (count, millions)", 
                       labels = label_comma(),
                       option = "C",
                       direction = -1,
                       limits = c(0, 1400),
                       breaks = seq(0, 1400, by = 200),
                       guide = guide_colorbar(
                         barheight = unit(0.5, "cm"))
                       ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.key.size = unit(1.5, "cm")
  ) +
  labs(title = "World Map with Population, 2015",
       subtitle = "Data from United Nations") +
  theme(legend.position = "bottom")

ggsave(filename = "C:/Users/mayaol/professional_dev/portfolio/analytics/analysis_1_worldmap.jpg", 
       plot = un_pop_map, width = 10, height = 7)