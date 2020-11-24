## Script to make a map
## of all 1581 cities used in the analysis

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(scales)
library(mgcv)
library(ggcorrplot)
library(corrplot)
library(arm)
library(mgcViz)
library(tibble)
library(mgcv.helper)
library(patchwork)
library(sf)

source("R/global_functions.R")

## read in data
# response variables
load("Data for modelling/response_variables.RData")

# checklist-specific predictors
load("Data for modelling/checklist_specific_variables.RData")

# city-specific data
load("Data for modelling/city_specific_data.RData")

# join the data
df <- response_variables %>% 
  inner_join(., checklist_specific_variables, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., city_specific_df, by="city")

# read in a dataset of random polygons
random_polys <- readRDS("Data/random_polygon_richness_summary.RDS")


## Look at relationship between total richness
## and city size
df2 <- df %>%
  group_by(city) %>%
  summarise(mean_richness=mean(species_richness),
            sd_richness=sd(species_richness)) %>%
  inner_join(., city_specific_df, by="city")



## read in all_cities geojson
geojson_trimmed <- read_sf("Spatial data/all_cities.geojson") %>%
  rename(city = NAME10) %>%
  mutate(city = gsub(", ", "_", .$city)) %>%
  dplyr::select(city, geometry) %>%
  right_join(., df2, by="city")

## us shapefile
us <- read_sf("Spatial data/United States Shapefile/United_States_Shapefile.shp")


ggplot()+
  geom_sf(data=us, fill="gray95")+
  geom_sf(data=geojson_trimmed, fill='black', color='black')+
  theme_classic()+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_blank())+
  theme(panel.grid.major=element_line(color="transparent"))+
  theme(panel.border=element_blank())

ggsave(filename = "Figures/map_of_1581_cities.png", width=8, height=5.5, units="in")




