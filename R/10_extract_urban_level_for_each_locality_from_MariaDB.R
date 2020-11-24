# An R script to retrieve the
# 'urbanness' associated with each checklist, based on
# the nighttime lights values associated
# the nighttimes lights was calculated in google earth engine
# and then put into the MariaDB
# this script first gets a list of all unique "LOCALITY_ID"
# from the subset of cities which have checklist-specific response variables
# then extracts these values from the MariaDB for each city at a time
# NOTE!!!!! This script takes forever to run and the MariaDB will frequently time out causing errors
# etc. So, I had to manually do it and restart a number of times to get through all potential cities


# packages
library(dbplyr)
library(dplyr)
library(RMariaDB)
library(purrr)


# function to 
# connect to MariaDB
get_con  <- function () {
  dbConnect(RMariaDB::MariaDB(), host = 'KESTREL', dbname='ebird',user = Sys.getenv('userid'), password = Sys.getenv('pwd'))
} 

# function to extract the necessary data for
# a given locality_ID
extract_urban_level_function <- function(x) {

message(x)
  
locality_list <- readRDS(paste0("eBird data/checklist specific response variables/", x)) %>%
  dplyr::select(LOCALITY_ID) %>%
  distinct() %>%
  .$LOCALITY_ID
  
  
con <- get_con()
sites <- tbl(con, 'sites')
rad_5k <- tbl(con, 'rad5k')

location_urban <- sites %>% 
  left_join(rad_5k) %>% 
  select(LOCALITY_ID, avg_rad_count_5k, avg_rad_mean_5k, avg_rad_median_5k) %>%
  filter(LOCALITY_ID %in% locality_list) %>% 
  collect(n=Inf)

}

# get list of files and then apply function across list
# then get df of results
files <- list.files("eBird data/checklist specific response variables/")

for (i in unique(files)) {
  tryCatch({
    
    df <- extract_urban_level_function(i)
    
    city <- gsub(".RDS", "", i)
    
    message(paste0("Analyzing ", city))
    
    saveRDS(df, paste0("checklist_level_urbanness/", city, ".RDS"))
  }, 
  error=function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  }) 
  
}







