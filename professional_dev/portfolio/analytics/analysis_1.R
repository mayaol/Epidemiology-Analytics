# For my first analysis, I will investigate the UN Population dataset and
# create several visualizations

library(tidyverse)

# 1. Investigate dataset
un_data <- read.csv("C:/Users/mayaol/professional_dev/portfolio/data_sources/un_pop_08_15_2024.csv")
un_cols <- data.frame(colnames(un_data))


# 2. World map of population density