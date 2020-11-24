library(sf)
library(dplyr)
library(units)

usa <- st_read("Spatial data/United States Shapefile/United_States_Shapefile.shp")

usa <- st_transform(usa, crs=4269)

usa <- st_transform(usa, crs="+proj=utm +zone=42N +datum=WGS84 +units=km")

# city-specific data
load("Data for modelling/city_specific_data.RData")

dist <- city_specific_df %>%
  dplyr::select(ALAND10) %>%
  mutate(radius_dist = sqrt((ALAND10/1000)/pi))


for (i in 1:5000) {
  
  random_pt <- st_sample(usa, 1)
  
  if(length(random_pt) == 1){
  
  buffer_dist <- sqrt(sample_n(dist, 1)[1][[1]]/pi)/1000
  
  a <- st_buffer(random_pt, dist=buffer_dist)
  
  print(st_area(a))
  
  b <- st_transform(a, crs=4269)
  
  print(set_units(st_area(b), km^2))
  
  st_write(b, dsn=paste0("Spatial data/random polygon geojson/random_poly_", i, ".geojson"))
  
  } else {
    print("Random point not found")
  }
  
}

