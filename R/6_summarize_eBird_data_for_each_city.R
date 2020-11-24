# AN R script to summarize all the rds files for each city multiple ways
# the first way is to provide a summary specific to each city, which includes:
# total richness, total number of lists, BCR code, total lists per season
# total richness per season

# packages
library(dplyr)
library(tidyr)
library(readr)

source("R/global_functions.R")

# Read in clements checklist and subset to
# just species names and order
# this will be used to remove some orders before calculating 
# checklist-specific measures
clements <- read_csv("eBird data/Clements-Checklist-v2018-August-2018.csv") %>%
  dplyr::filter(category == "species") %>%
  rename(COMMON_NAME = `English name`) %>%
  dplyr::select(COMMON_NAME, order, family)

# function which takes a city rds file input
# and summarizes it at the level of the city
# calculates a handful of things stored in "city_summary"
city_eBird_summary <- function (x) {
  
  df <- readRDS(paste0("eBird data/rds for each city/", x)) %>%
    mutate(season=getSeason(OBSERVATION_DATE)) %>%
    left_join(., clements, by="COMMON_NAME") %>%
    dplyr::filter(family != "Stercorariidae (Skuas and Jaegers)") %>%
    dplyr::filter(family != "Alcidae (Auks, Murres, and Puffins)") %>%
    dplyr::filter(family != "Diomedeidae (Albatrosses)") %>%
    dplyr::filter(family != "Oceanitidae (Southern Storm-Petrels)") %>%
    dplyr::filter(family != "Hydrobatidae (Northern Storm-Petrels)") %>%
    dplyr::filter(family != "Procellariidae (Shearwaters and Petrels)") %>%
    dplyr::filter(family != "Fregatidae (Frigatebirds)") %>%
    dplyr::filter(family != "Sulidae (Boobies and Gannets)")
  
  city_name <- gsub(".RDS", "", x)
  
  summary <- df %>%
    summarise(total_richness=length(unique(.$COMMON_NAME)),
              total_lists=length(unique(.$SAMPLING_EVENT_IDENTIFIER)),
              BCR=Mode(.$BCR_CODE))
  
  empty_df_season <- data.frame(season=c("Autumn", "Spring", "Summer", "Winter"),
                                richness=c(NA, NA, NA, NA),
                                lists=c(NA, NA, NA, NA))
  
  if (length(unique(df$season)) < 4) {
    df <- df %>% right_join(., empty_df_season)
  } else {
    df
  }
  
  
  summary_season <- data.frame(Autumn_richness=length(unique(na.omit(filter(df, season == "Autumn")$COMMON_NAME))),
                               Spring_richness=length(unique(na.omit(filter(df, season == "Spring")$COMMON_NAME))),
                               Summer_richness=length(unique(na.omit(filter(df, season == "Summer")$COMMON_NAME))),
                               Winter_richness=length(unique(na.omit(filter(df, season == "Winter")$COMMON_NAME))),
                               Autumn_lists=length(unique(na.omit(filter(df, season == "Autumn")$SAMPLING_EVENT_IDENTIFIER))),
                               Spring_lists=length(unique(na.omit(filter(df, season == "Spring")$SAMPLING_EVENT_IDENTIFIER))),
                               Summer_lists=length(unique(na.omit(filter(df, season == "Summer")$SAMPLING_EVENT_IDENTIFIER))),
                               Winter_lists=length(unique(na.omit(filter(df, season == "Winter")$SAMPLING_EVENT_IDENTIFIER)))
                               )

  
  city_summary <- bind_cols(summary, summary_season) %>%
    mutate(NAME10=city_name) %>%
    dplyr::select(NAME10, everything())
  
  return(city_summary)
  
}


## Now apply the above function: city_eBird_summary to a list of rds files
## then unlist the list and write out as a city-specific-summary-for-all-cities
files <- list.files("eBird data/rds for each city/")

list <- lapply(files, function(x) city_eBird_summary(x))

city_df <- do.call(rbind.data.frame, list)


saveRDS(city_df, "eBird data/city_specific_summary_richness_and_lists_all_cities.RDS")



