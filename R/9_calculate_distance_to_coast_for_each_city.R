#### packages
library(readr)
library(rgdal)
library(rgeos)
library(sf)

#### read file in
load("Spatial data/cities_for_analysis.RData")

#### calculate nearest distance to coast
pts <- analysis_cities[, c('lng', 'lat')]



wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sp.points <- SpatialPoints(pts, proj4string=CRS(wgs.84))




coast  <- readOGR(dsn="Spatial data/ne_10m_coastline",layer="ne_10m_coastline",p4s=wgs.84)
coast.moll <- spTransform(coast,CRS(mollweide))
point.moll <- spTransform(sp.points,CRS(mollweide))


analysis_cities$distance.to.coast_km <- (sapply(1:length(point.moll), function(i)gDistance(point.moll[i],coast.moll)))/1000



save(analysis_cities, file="Spatial data/cities_for_analysis.RData")



