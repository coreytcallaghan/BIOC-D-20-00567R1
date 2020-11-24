# an R script to download read in all geojsons and combine to one sf
# then write it out as a geojson
# this will be handy to make plots and figures later on either interactive or static

# packages
library(sf)

files <- list.files("Spatial data/City geojson")


list <- lapply(files, function(x) st_read(paste0("Spatial data/City geojson/", x)))


sf_df_combined <- sf::st_as_sf(data.table::rbindlist(list))


st_write(sf_df_combined, dsn="Spatial data/all_cities.geojson", 
         layer_options = "GEOMETRY=AS_WKT")


