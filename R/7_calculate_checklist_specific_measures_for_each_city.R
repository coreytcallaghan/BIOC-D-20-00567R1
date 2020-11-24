## An R script to extend the city-specific summary script
## the purpose of this script is to again summarize the eBird lists
## within a city, but at the level of an eBird checklist within each city
## meaning that each city will have a different number of
## observations, based on how many eBird lists have been submitted for that city
## Note that this script follows a similar approach to the script: 
## "6_summarize_eBird_data_for_each_city.R

# packages
library(dplyr)
library(vegan)
library(tidyr)
library(geosphere)
library(data.table)
library(readr)
library(parallel)
library(tibble)
library(picante)

source("R/global_functions.R")


# Read in clements checklist and subset to
# just species names and order
# this will be used to remove some orders before calculating 
# checklist-specific measures
clements <- read_csv("eBird data/Clements-Checklist-v2018-August-2018.csv") %>%
  dplyr::filter(category == "species") %>%
  rename(COMMON_NAME = `English name`) %>%
  dplyr::select(COMMON_NAME, order, family)


# Read in city-metadata which has
# coordinates of each city and subset to just city
# name and the lat/long
load("Spatial data/cities_for_analysis.RData")

analysis_cities <- analysis_cities %>%
  dplyr::select(NAME10, lat, lng) %>%
  distinct(.) %>%
  droplevels(.) %>%
  rename(city_lat=lat) %>%
  rename(city_lng=lng) %>%
  mutate(NAME10 = gsub(", ", "_", .$NAME10))

# read in tree
# too large to store in repository
# can be downloaded from here: https://birdtree.org/
tree <- read.tree(file="Phylo data/phy.tre", keep.multi=FALSE, tree.names=TRUE)[[1]]

# read in taxo key
key <- read_csv("Phylo data/eBird_jetz_taxo_key.csv")

####################################
##################### FUNCTION

calculate_checklist_response_variables <- function(x) {
  

# read in dataframe
# join with clements
# and then remove any seabirds families from potential downstream analyses
df <- readRDS(paste0("eBird data/rds for each city/", x)) %>%
  mutate(season=getSeason(OBSERVATION_DATE)) %>%
  mutate(NAME10 = gsub(".RDS", "", x)) %>%
  left_join(., clements, by="COMMON_NAME") %>%
  dplyr::filter(family != "Stercorariidae (Skuas and Jaegers)") %>%
  dplyr::filter(family != "Alcidae (Auks, Murres, and Puffins)") %>%
  dplyr::filter(family != "Diomedeidae (Albatrosses)") %>%
  dplyr::filter(family != "Oceanitidae (Southern Storm-Petrels)") %>%
  dplyr::filter(family != "Hydrobatidae (Northern Storm-Petrels)") %>%
  dplyr::filter(family != "Procellariidae (Shearwaters and Petrels)") %>%
  dplyr::filter(family != "Fregatidae (Frigatebirds)") %>%
  dplyr::filter(family != "Sulidae (Boobies and Gannets)")


# select everything that is checklist-specific and save as df to merge back later
sampling_event_info <- df %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, NAME10, LOCALITY_ID,
                OBSERVATION_DATE, PROTOCOL_TYPE, EFFORT_DISTANCE_KM,
                EFFORT_AREA_HA, DURATION_MINUTES, LATITUDE, LONGITUDE,
                OBSERVER_ID, season, number_X) %>%
  distinct(.) %>%
  # add the city lat/city lng into the data frame
  left_join(., analysis_cities, by="NAME10")

# calculate the distance each checklist is from the city lat/lng
setDT(sampling_event_info)[, distance.km := distGeo(matrix(c(city_lng, city_lat), ncol=2),
                                                      matrix(c(LONGITUDE, LATITUDE), ncol=2))/1000]


# calculate species richness and species diversity on a checklist
biodiversity_data <- df %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(species_richness=length(unique(COMMON_NAME)),
            shannon_diversity=diversity(OBSERVATION_COUNT),
            abundance=sum(OBSERVATION_COUNT))


# calculate PD on a checklist
pd <- df %>%
  as.data.frame() %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, OBSERVATION_COUNT) %>%
  # this gives any species which was present a 1
  replace_na(list(OBSERVATION_COUNT=1)) %>%
  # merge with key and replace COMMON_NAME with TipLabel
  # to match the phylo object
  left_join(., key, by="COMMON_NAME") %>%
  dplyr::select(TipLabel, SAMPLING_EVENT_IDENTIFIER, OBSERVATION_COUNT) %>%
  group_by(TipLabel, SAMPLING_EVENT_IDENTIFIER) %>%
  # This fixes an issue where two 'species' are on the same checklist
  # not accordin to clements, but according to the taxonomy of Jetz
  summarise(OBSERVATION_COUNT = sum(OBSERVATION_COUNT)) %>%
  spread(TipLabel, OBSERVATION_COUNT, fill=0) %>%
  column_to_rownames(var="SAMPLING_EVENT_IDENTIFIER") %>%
  as.matrix.data.frame() %>%
  picante::pd(., tree) %>%
  as.data.frame() %>%
  rownames_to_column(var="SAMPLING_EVENT_IDENTIFIER") %>%
  dplyr::select(-SR)


checklist_responses <- biodiversity_data %>%
  inner_join(., pd, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER")

return(checklist_responses)

}

# now run the above function for all the cities in the analysis
# note though that this function won't work for a city with no data!
# so I'll first subset the list of files to that which has some data
# I'll make the cutoff > 50 checklists for now
# but will come back to this whenever the time is necessary
# need to read in the city_df which was calculated in script #6 first though
# which has the number of total lists for each city in it

# read in city data and subset to a list of cities which have greater than 50 checklists
files <- readRDS("eBird data/city_specific_summary_richness_and_lists_all_cities.RDS") %>%
  dplyr::filter(total_lists >= 50) %>%
  mutate(add=".RDS") %>%
  unite(NAME10, NAME10, add, sep="") %>%
  .$NAME10



for (i in unique(files)) {
  tryCatch({
    
    df <- calculate_checklist_response_variables(i)
    
    city <- gsub(".RDS", "", i)
    
    message(paste0("Analyzing ", city))
    
    saveRDS(df, paste0("eBird data/checklist specific response variables/", city, ".RDS"))
  }, 
  error=function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  }) 
  
}



# parallelize the analysis of each element in the list
# by parallelizing lapply
# using the parLapply approach (see here: http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html)
# because of an error for some, this did not work previously
# but that error was fixed so should work now
# will leave this here for future referene and if I need to redo it all
#cl <- makeCluster(10)

#clusterExport(cl, c("files", "analysis_cities", "clements", 
#                    "calculate_checklist_response_variables", "getSeason",
#                    "key", "tree"))

#clusterEvalQ(cl, {library(dplyr)
#                  library(vegan)
#                  library(tidyr)
#                  library(geosphere)
#                  library(data.table)
#                  library(readr)
#                  library(tibble)
#                  })

#list <- parLapply(cl, files, function(x) calculate_checklist_response_variables(x))

#response_variables_df <- do.call(rbind.data.frame, list)

# save the response variables dataframe
#saveRDS(response_variables_df, "eBird data/checklist_specific_response_variables.RDS")

#if (T) {
  # files <- list.files("Spatial data/City geojson")
#  files <- list.files('Spatial data/City geojson/', full.names = TRUE)
  
#}






