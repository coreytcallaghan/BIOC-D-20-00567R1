## To run these commands you need to connect to a ebird database, setup like in the database folder:
## and have an .Renviron file with your database credentials 
## follow the steps outlined here: 
## read the 
## https://db.rstudio.com/best-practices/managing-credentials/#use-environment-variables
options(readr.show_progress = FALSE)
library(dbplyr)
library(dplyr, quietly = T, warn.conflicts = F)
library(RMariaDB)
library(geojsonR)
library(wellknown)
library(tidyr)
library(sf)

## establish connection to MaraDB
get_con  <- function () {
  dbConnect(RMariaDB::MariaDB(), host = 'KESTREL', dbname='ebird',user = Sys.getenv('userid'), password = Sys.getenv('pwd'))
} 



## this is the function used to clean spatially select all checklists within
## a city geojson and then clean the data for each city based on a suite of
## rules used elsewhere and previously
samples_from_file <- function (file) {
  con <- get_con()
  wkt <- file %>% st_read %>% st_geometry %>% st_as_text
  query_ <- paste0(
    "
    SELECT
    *
    FROM checklists as lists, sites as sites, samples as samples, species as species
    WHERE
    samples.TAXONOMIC_ORDER = species.TAXONOMIC_ORDER
    AND
    samples.SAMPLING_EVENT_IDENTIFIER = lists.SAMPLING_EVENT_IDENTIFIER
    AND
    lists.LOCALITY_ID = sites.LOCALITY_ID 
    AND
    MBRContains( GeomFromText(
    \'", wkt,"\'
    ), sites.pt);")
  lists <- dbGetQuery(con, query_)
  data <- as.data.frame(lists)
  
  ## clean the spatially queried data to that which I want to use for analysis
  ## First step is to clean the data and limit the checklists used in the analysis
  # here is where you add all the columns needed for the analysis (that don't vary within checklist)
  sampling_event_info <- data %>%
    select(SAMPLING_EVENT_IDENTIFIER, LOCALITY_ID, OBSERVATION_DATE, TIME_OBSERVATIONS_STARTED,
           PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, EFFORT_AREA_HA,
           DURATION_MINUTES, GROUP_IDENTIFIER, LATITUDE, LONGITUDE, OBSERVER_ID, COUNTRY, COUNTRY_CODE, BCR_CODE) %>%
    distinct()
  
  # Counts how many 'x's per checklist
  X_missing <- data %>%
    group_by(SAMPLING_EVENT_IDENTIFIER) %>%
    summarise(number_X = sum(OBSERVATION_COUNT=="X"))
  
  # accounts for the instance in which people submit 
  # the same species both at the species and subspecies level
  # also makes it so only 'species' and 'issf' category are included in analysis
  clean_data <- data %>%
    filter(CATEGORY %in% c("species", "issf", "domestic")) %>% 
    group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>%
    summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
    rename(OBSERVATION_COUNT = COUNT_SPP) %>% 
    inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER") %>%
    inner_join(., X_missing, by="SAMPLING_EVENT_IDENTIFIER")
  
  # applying some filtering criteria
  analysis_data <- clean_data %>%
    dplyr::filter(ALL_SPECIES_REPORTED == 1, # only complete checklists
                  PROTOCOL_TYPE !="Incidental", # get rid of 'incidental' lists
                  PROTOCOL_TYPE !="Historical") %>% # get rid of 'historical' lists
    # get rid of any checklists prior to 2010 - rather arbitrary cutoff, but gets rid of older stuff a bit
    filter(OBSERVATION_DATE >= "2010-01-01") %>%
    dplyr::filter(DURATION_MINUTES >= 5) %>%
    dplyr::filter(DURATION_MINUTES <= 240) %>%
    replace_na(list(EFFORT_DISTANCE_KM = 0)) %>%
    dplyr::filter(EFFORT_DISTANCE_KM <= 5) %>%
    replace_na(list(EFFORT_AREA_HA = 0)) %>%
    dplyr::filter(as.numeric(as.character(EFFORT_AREA_HA)) <= 500) %>%
    ungroup()
  
  # first select the group_identifiers and associated checklists
  duplicated <- analysis_data %>%
    dplyr::select(GROUP_IDENTIFIER, SAMPLING_EVENT_IDENTIFIER) %>%
    distinct(.keep_all=TRUE) %>%
    filter(grepl("G", GROUP_IDENTIFIER)) %>%
    group_by(GROUP_IDENTIFIER) %>%
    # randomly sample one checklist for each group_identifier
    sample_n(., 1) %>%
    .$SAMPLING_EVENT_IDENTIFIER
  
  duplicated_data <- analysis_data %>%
    filter(SAMPLING_EVENT_IDENTIFIER %in% duplicated)
  
  ## now, append the selected checklists for each group_identifier
  ## with the non group_identifier checklists from the data
  # create analysis_data.all for all data
  analysis_data <- analysis_data %>% 
    filter(!grepl("G", GROUP_IDENTIFIER)) %>%
    bind_rows(., duplicated_data) %>%
    dplyr::select(-GROUP_IDENTIFIER)
  
  return(analysis_data)
  
  dbDisconnect(con)
}


## Now use a for loop to loop through all cities in the geojson list and
## extract the eBird data for each city and write out
if (T) {
  # files <- list.files("Spatial data/City geojson")
  files <- list.files('Spatial data/random polygon geojson/', full.names = TRUE)
  
  for (i in unique(files)) {
    tryCatch({
      print(i)
      df <- samples_from_file(i)
      
      poly <- gsub(".*random polygon geojson/|.geojson$", '', i)
      
      message(paste0("Analysing ", poly))
      
      saveRDS(df, paste0("eBird data/rds for each random poly/", poly, ".RDS"))
    }, 
    error=function(e){
      cat("ERROR :",conditionMessage(e), "\n")
    }) 
    
  }
  
}