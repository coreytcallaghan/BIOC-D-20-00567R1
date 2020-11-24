## An R script to prepare an appendix for submission
## of the cities included and how whether they were
## included in the analysis

# packages
library(readr)
library(dplyr)
library(tidyr)

# city-specific data
load("Data for modelling/city_specific_data.RData")


df <- city_specific_df %>%
  dplyr::select(city, state_name, lat, lng, ALAND10, population, distance.to.coast_km, total_richness, total_lists) %>%
  mutate(Area_km2=ALAND10/1000) %>%
  dplyr::select(-ALAND10) %>%
  arrange(Area_km2) %>%
  mutate(Included_in_analysis= case_when(
    total_lists >= 50 ~ "Yes",
    total_lists < 50 ~ "No"))

write_csv(df, "Data/city_data_for_appendix.csv")
