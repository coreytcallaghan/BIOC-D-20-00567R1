### An R script to split all the polygons
### for each city to include in the analysis
### see script: "1_get_list_of_cities_to_include.R"
### This will make it easier to loop through each city
### and calculate the necessary information for each city (birds and habitat stuff)

### packages
library(sf)

### First read in the shapefile
cities_shp <- st_read("Spatial data/Original spatial data/census_bureau_2017-urban_clusters_and_areas")

### read in list of cities for analysis as we don't want every city
load("Spatial data/cities_for_analysis.RData")

### filter shapefile by cities to use
cities_shp <- cities_shp %>%
  dplyr::select(NAME10) %>%
  right_join(., analysis_cities, by="NAME10")

### for loop which writes out each single polygon, split by NAME10 (urban cluster or area)
### as its own geojson
### this is good for later as we have a script to adapt which splits geojson into
### wkt and then does spatial intersection of eBird SQL
for (i in unique(cities_shp$NAME10)) {
  
  name <- gsub(", ", "_", i)
  
  df <- cities_shp %>%
    filter(NAME10 == i)
  
  st_write(df, dsn=paste0("Spatial data/City geojson/", name, ".geojson"), 
           layer_options = "GEOMETRY=AS_WKT")
  
}




