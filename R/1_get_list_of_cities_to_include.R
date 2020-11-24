#### An R script to get cities to include in city-specific comparison analysis
#### gets metadata from US Census bureau file's polygons of urban clusters and areas
#### and joins it with a list of US cities
#### this provides us with a list of which cities we would have potential data for

### packages
library(readr)
library(dplyr)
library(tidyr)

### read in polygon metadata
census_bureau_metadata <- read_csv("Spatial data/Original spatial data/census_bureau_metadata.txt")

# would like to limit the polygons to those which only have
# one 'city'. this removes multiple city clusers
# for example (New york and Newark are combined into one urban area)
# would like to remove these to keep it a bit more simplistic in terms of presentation of results
# also gets rid of cities which are split across states
# this makes sense sociopolitically maybe?
# will probably make it easier to then somehow get socioeconomic status if
# only include cities which are entirely within a specific city
census_bureau_metadata <- census_bureau_metadata[!grepl("--", census_bureau_metadata$NAME10),]

### read in large list of US cities
### this list contains population information for the majority of cities
### more information on this dataset: https://simplemaps.com/data/us-cities
us_cities <- read_csv("Spatial data/Original spatial data/uscitiesv1.4.csv")


# join the city name with the state abbrevation
# then merge with the census bureau metadata to see how
# many cities we are left with
# lastly, remove any cities which aren't in BOTH datasets
analysis_cities <- us_cities %>%
  unite(NAME10, city, state_id, sep=", ") %>%
  right_join(., census_bureau_metadata, by="NAME10") %>%
  na.omit(.) %>%
# select necessary columns
  dplyr::select(1:10, 16:23) %>%
## also get rid of any cities which are in Alaska, as we will restrict our analysis to
## contiguous United States
  dplyr::filter(state_name != "Alaska")



# write out cities for analysis as .RData file
save(analysis_cities, file="Spatial data/cities_for_analysis.RData")

  
  
  
  
  
  