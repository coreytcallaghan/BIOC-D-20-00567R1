### This script is to apply
### a gam to each city that includes
### gls_water and gls_tree and evi
### at the checklist level
### trying to explain why urbanization is not an important
### predictor at the city-level!
### Follows the same approach as the script: "apply_gam_to_each_city.R"

## packages
library(dplyr)
library(tidyr)
library(mgcv)
library(ggplot2)
library(broom)
library(purrr)
library(mgcv.helper)

source("R/global_functions.R")

## read in data
# response variables
load("Data for modelling/response_variables.RData")

# checklist-specific predictors
load("Data for modelling/checklist_specific_variables.RData")

# city-specific data
load("Data for modelling/city_specific_data.RData")

# load checklist_predictors
# and rename and select the necessary
# predictors
# need to check this with John/Mitch
checklist_predictors <- read_csv("Data/checklist_level_stats_cleaned.csv") %>%
  mutate(evi_mean=(evi_2013_mean + evi_2014_mean + evi_2015_mean + evi_2016_mean + evi_2017_mean + evi_2018_mean/6)) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, gls_water_mean, 
                impervious_mean, tree_canopy_cover_mean, evi_mean) %>%
  rename(mean_EVI.list = evi_mean) %>%
  rename(tree_mean.list = tree_canopy_cover_mean) %>%
  rename(proportion_water.list = gls_water_mean)
  

# join the data
df <- response_variables %>% 
  inner_join(., checklist_specific_variables, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., city_specific_df, by="city") %>%
  inner_join(., checklist_predictors, by="SAMPLING_EVENT_IDENTIFIER")


#########################################
#########################################
##### First for species richness ########
#########################################
#########################################
hist(df$species_richness)
hist(log(df$species_richness))


apply_gam <- function(city_name){
  
  model_data <- df %>%
    dplyr::filter(city==city_name) %>%
    mutate(season2 = case_when(season=="Autumn" ~ 1,
                               season=="Winter" ~ 2,
                               season=="Spring" ~ 3,
                               season=="Summer" ~ 4)) %>%
    mutate(city = as.factor(as.character(city))) %>% 
    mutate_each(funs(scale), urbanness, proportion_water.list, tree_mean.list, mean_EVI.list)
  
  mod <- mgcv::gam(log(species_richness) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + s(LATITUDE, LONGITUDE) +
                     s(season2, bs="cc", k=4) + urbanness + tree_mean.list + 
                     proportion_water.list + mean_EVI.list, data=model_data)

  
  temp <- plot.gam(mod, page=1)
  
  summary_data <- confint(mod) %>%
    rename(lower="2.5%") %>%
    rename(upper="97.5%") %>%
    mutate(city=city_name) %>%
    mutate(p=summary(mod)$p.pv) %>%
    mutate(significance=case_when(
      p<=0.05 ~ "Yes",
      p>0.05 ~ "No"
    )) %>%
    mutate(deviance=summary(mod)$dev.exp)
  
  return(summary_data)
  
}

FaultTolerant_gam_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam(city_name);}, error=function(e) {ret <<- data.frame(city=paste0(city_name))});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/gam_richness_results_parametric_model_df.RDS")


#########################################
#########################################
##### Now for species diversity ########
#########################################
#########################################
hist(na.omit(df$shannon_diversity))


apply_gam_shannon <- function(city_name){
  
  model_data <- df %>%
    dplyr::filter(city==city_name) %>%
    mutate(season2 = case_when(season=="Autumn" ~ 1,
                               season=="Winter" ~ 2,
                               season=="Spring" ~ 3,
                               season=="Summer" ~ 4)) %>%
    mutate(city = as.factor(as.character(city))) %>% 
    mutate_each(funs(scale), urbanness, proportion_water.list, tree_mean.list, mean_EVI.list)
  
  mod <- mgcv::gam(shannon_diversity ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + s(LATITUDE, LONGITUDE) +
                     s(season2, bs="cc", k=4) + urbanness + tree_mean.list + 
                     proportion_water.list + mean_EVI.list, data=model_data)
  
  
  temp <- plot.gam(mod, page=1)
  
  summary_data <- confint(mod) %>%
    rename(lower="2.5%") %>%
    rename(upper="97.5%") %>%
    mutate(city=city_name) %>%
    mutate(p=summary(mod)$p.pv) %>%
    mutate(significance=case_when(
      p<=0.05 ~ "Yes",
      p>0.05 ~ "No"
    )) %>%
    mutate(deviance=summary(mod)$dev.exp)
  
  return(summary_data)
  
}

FaultTolerant_gam_shannon_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam_shannon(city_name);}, error=function(e) {ret <<- data.frame(city=paste0(city_name))});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_shannon_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/gam_shannon_results_parametric_model_df.RDS")


#########################################
#########################################
##### Now for total abundance ########
#########################################
#########################################
hist(na.omit(df$abundance))
hist(na.omit(log(df$abundance)))


apply_gam_abundance <- function(city_name){
  
  model_data <- df %>%
    dplyr::filter(city==city_name) %>%
    mutate(season2 = case_when(season=="Autumn" ~ 1,
                               season=="Winter" ~ 2,
                               season=="Spring" ~ 3,
                               season=="Summer" ~ 4)) %>%
    mutate(city = as.factor(as.character(city))) %>% 
    mutate_each(funs(scale), urbanness, proportion_water.list, tree_mean.list, mean_EVI.list)
  
  mod <- mgcv::gam(log(abundance) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + s(LATITUDE, LONGITUDE) +
                     s(season2, bs="cc", k=4) + urbanness + tree_mean.list + 
                     proportion_water.list + mean_EVI.list, data=model_data)
  
  
  temp <- plot.gam(mod, page=1)
  
  summary_data <- confint(mod) %>%
    rename(lower="2.5%") %>%
    rename(upper="97.5%") %>%
    mutate(city=city_name) %>%
    mutate(p=summary(mod)$p.pv) %>%
    mutate(significance=case_when(
      p<=0.05 ~ "Yes",
      p>0.05 ~ "No"
    )) %>%
    mutate(deviance=summary(mod)$dev.exp)
  
  return(summary_data)
  
  
}

FaultTolerant_gam_abundance_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam_abundance(city_name);}, error=function(e) {ret <<- data.frame(city=paste0(city_name))});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_abundance_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/gam_abundance_results_parametric_model_df.RDS")



#########################################
#########################################
##### Now for phylo diversity ###########
#########################################
#########################################
hist(na.omit(df$PD))
hist(na.omit(log(df$PD)))


apply_gam_phylo <- function(city_name){
  
  model_data <- df %>%
    dplyr::filter(city==city_name) %>%
    mutate(season2 = case_when(season=="Autumn" ~ 1,
                               season=="Winter" ~ 2,
                               season=="Spring" ~ 3,
                               season=="Summer" ~ 4)) %>%
    mutate(city = as.factor(as.character(city))) %>% 
    mutate_each(funs(scale), urbanness, proportion_water.list, tree_mean.list, mean_EVI.list)
  
  mod <- mgcv::gam(log(PD) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + s(LATITUDE, LONGITUDE) +
                     s(season2, bs="cc", k=4) + urbanness + tree_mean.list + 
                     proportion_water.list + mean_EVI.list, data=model_data)
  
  
  temp <- plot.gam(mod, page=1)
  
  summary_data <- confint(mod) %>%
    rename(lower="2.5%") %>%
    rename(upper="97.5%") %>%
    mutate(city=city_name) %>%
    mutate(p=summary(mod)$p.pv) %>%
    mutate(significance=case_when(
      p<=0.05 ~ "Yes",
      p>0.05 ~ "No"
    )) %>%
    mutate(deviance=summary(mod)$dev.exp)
  
  return(summary_data)
  
}

FaultTolerant_gam_phylo_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam_phylo(city_name);}, error=function(e) {ret <<- data.frame(city=paste0(city_name))});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_phylo_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/gam_phylo_results_parametric_model_df.RDS")














