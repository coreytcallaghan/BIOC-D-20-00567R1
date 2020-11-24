# This is an R script to pull together all the random disparate pieces
# and create a few .RData files necessary for all the modelling
# this should be the last script used to create dataframes necessary for
# modelling and then save those dataframes as pieces as necessary
# will be important to save them to be small enough (<100 MB)
# to be pushed through sourcetree with as little headache as possible


# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(readr)

# first get all the response variables together
# at the checklist level
setwd("eBird data/checklist specific response variables")

response_variables <- list.files(pattern=".RDS") %>%
  map_dfr(readRDS) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, species_richness, 
                shannon_diversity, abundance, PD)

setwd("./..")
setwd("./..")

save(response_variables, file="Data for modelling/response_variables.RData")

# create a dataframe with checklist-specific information for modelling
# first read in all checklist_level_urbanness files
# which will need to be joined by LOCALITY_ID
setwd("checklist_level_urbanness")

urban_level <- list.files(pattern=".RDS") %>%
  map_dfr(readRDS) %>%
  distinct()

setwd("./..")


# now read in all files from response variables folder
# and select the ones needed
setwd("eBird data/checklist specific response variables")

checklist_specific_variables <- list.files(pattern=".RDS") %>%
  map_dfr(readRDS) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, DURATION_MINUTES,
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, LATITUDE,
                LONGITUDE, season, NAME10, LOCALITY_ID, distance.km) %>%
  rename(city=NAME10) %>%
  rename(distance_from_city_center=distance.km) %>%
  left_join(., urban_level, by="LOCALITY_ID") %>%
  dplyr::select(-avg_rad_count_5k, -avg_rad_median_5k) %>%
  rename(urbanness=avg_rad_mean_5k)

setwd("./..")
setwd("./..")

save(checklist_specific_variables, file="Data for modelling/checklist_specific_variables.RData")


## Now prepare city-specific data
city_lists_df <- readRDS("eBird data/city_specific_summary_richness_and_lists_all_cities.RDS") %>%
  rename(city=NAME10)

load("Spatial data/cities_for_analysis.RData")


## read in google earth engine stuff
globcover <- read_csv("City level GEE data/esa_globcover_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  dplyr::select(city, mode) %>%
  rename(globcover_mode = mode)


gls_tree <- read_csv("City level GEE data/gls_tree_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  dplyr::select(city, mean) %>%
  rename(tree_mean = mean)

gls_water <- read_csv("City level GEE data/gls_water_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  dplyr::select(city, area_m2, area_value_m2) %>%
  mutate(proportion_water = area_value_m2/area_m2) %>%
  dplyr::select(city, proportion_water)

evi_2013 <- read_csv("City level GEE data/evi_2013_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  mutate(Year=2013) %>%
  dplyr::select(city, mean, Year)
evi_2014 <- read_csv("City level GEE data/evi_2013_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  mutate(Year=2014) %>%
  dplyr::select(city, mean, Year)
evi_2015 <- read_csv("City level GEE data/evi_2013_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  mutate(Year=2015) %>%
  dplyr::select(city, mean, Year)
evi_2016 <- read_csv("City level GEE data/evi_2013_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  mutate(Year=2016) %>%
  dplyr::select(city, mean, Year)
evi_2017 <- read_csv("City level GEE data/evi_2013_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  mutate(Year=2017) %>%
  dplyr::select(city, mean, Year)
evi_2018 <- read_csv("City level GEE data/evi_2013_hs.csv") %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  mutate(Year=2018) %>%
  dplyr::select(city, mean, Year)

evi <- bind_rows(evi_2013, evi_2014,
                 evi_2015, evi_2016,
                 evi_2017, evi_2018) %>%
  group_by(city) %>%
  summarise(mean_EVI = mean(mean))

city_specific_df <- analysis_cities %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10)) %>%
  rename(city=NAME10) %>%
  left_join(., city_lists_df, by="city") %>%
  left_join(., evi, by="city") %>%
  left_join(., gls_tree, by="city") %>%
  left_join(., gls_water, by="city") %>%
  left_join(., globcover, by="city")


save(city_specific_df, file="Data for modelling/city_specific_data.RData")


