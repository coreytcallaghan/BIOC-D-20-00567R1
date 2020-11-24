## An R script used to read in each of the city geojsons
## and then convert to sp and then make a 100 km buffer
## around the city geojson
## then write out the buffer as the potential species' pool buffer
library(sp)
library(geojsonio)
library(rgeos)
library(sf)


files <- list.files("spatial data/City geojson")

for (i in unique(files)) {
  tryCatch({
    file_sp <- geojsonio::geojson_read(paste0("Spatial data/City geojson/", i),
                                       what="sp")
    
    file_sf <- st_as_sf(file_sp)
    
    file_sp_trans <- spTransform(file_sp, CRS("+init=epsg:2163"))
    
    species_pool_sp_50 <- gBuffer(file_sp_trans, width=50000)
    species_pool_sp_100 <- gBuffer(file_sp_trans, width=100000)
    
    species_pool_sf_50 <- st_as_sf(species_pool_sp_50)
    species_pool_sf_100 <- st_as_sf(species_pool_sp_100)
    
    
    species_pool_sf_50 <- st_transform(species_pool_sf_50, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
    species_pool_sf_100 <- st_transform(species_pool_sf_100, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
    
    
    st_write(species_pool_sf_50, dsn=paste0("Spatial data/City species pool geojson/fifty km pool/", i), 
             layer_options = "GEOMETRY=AS_WKT")
    st_write(species_pool_sf_100, dsn=paste0("Spatial data/City species pool geojson/hundred km pool/", i), 
             layer_options = "GEOMETRY=AS_WKT")}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  
}








