## This script is to investigate the lat/lngs of the random polygons
## as raised by the reviewer in the review process
## they want to be sure that the random polygons
## follow similar lat/lng trends as the cities used in the analysis
## but I think I need to re-calculate the lat/lng of each of the polygons
## originally...
## and then join this up with the polygons that were actually used for the analysis

# packages
library(sf)
library(dplyr)


# first get a list of coordinates for the middle of each geojson random polygon
# list of polygon file names
setwd("Spatial data/random polygon geojson")

files <- list.files()

get_coords_function <- function(i) {
  
  dat <- st_read(i)
  
  coords <- as.data.frame(st_coordinates(st_centroid(dat))) %>%
    mutate(polygon=gsub(".geojson", "", i))
  
  return(coords)
  
}


poly_coords_list <- lapply(files, function(x){get_coords_function(x)})

poly_coords_df <- bind_rows(poly_coords_list)

setwd("..")
setwd("..")

saveRDS(poly_coords_df, "Data/random_poly_coords.RDS")





