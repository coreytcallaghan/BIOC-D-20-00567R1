## This is an R script to run a gam
## model for every city - the same gam model
## is specified for each city
## and then save the results out


## packages
library(dplyr)
library(tidyr)
library(mgcv)
library(ggplot2)
library(broom)
library(purrr)

source("R/global_functions.R")

## read in data
# response variables
load("Data for modelling/response_variables.RData")

# checklist-specific predictors
load("Data for modelling/checklist_specific_variables.RData")

# city-specific data
load("Data for modelling/city_specific_data.RData")

# join the data
df <- response_variables %>% 
  inner_join(., checklist_specific_variables, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., city_specific_df, by="city")


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
    mutate(city = as.factor(as.character(city)))
  
  mod <- mgcv::gam(log(species_richness) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + 
                     s(season2, bs="cc", k=4) + urbanness, data=model_data)
  
  estimate <- tidy(mod, parametric=TRUE) %>%
    dplyr::filter(term=="urbanness") %>%
    .$estimate
  
  temp <- plot.gam(mod, page=1)
  
  plotting_data <- data.frame(estimate=estimate) %>%
    mutate(city=paste0(city_name)) %>%
    mutate(r_squared=summary(mod)$r.sq)
  
  return(plotting_data)
  
}

FaultTolerant_gam_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam(city_name);}, error=function(e) {ret <<- data.frame(x=NA,
                                                                                 y=NA,
                                                                                 se=NA,
                                                                                 city=paste0(city_name),
                                                                                 edf=NA)});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/simple_gam_richness_results_df.RDS")


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
    na.omit(.)
  
  mod <- mgcv::gam(shannon_diversity ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + 
                     s(season2, bs="cc", k=4) + urbanness, data=model_data)
  
  estimate <- tidy(mod, parametric=TRUE) %>%
    dplyr::filter(term=="urbanness") %>%
    .$estimate
  
  temp <- plot.gam(mod, page=1)
  
  plotting_data <- data.frame(estimate=estimate) %>%
    mutate(city=paste0(city_name)) %>%
    mutate(r_squared=summary(mod)$r.sq)
  
  return(plotting_data)
  
}

FaultTolerant_gam_shannon_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam_shannon(city_name);}, error=function(e) {ret <<- data.frame(x=NA,
                                                                                         y=NA,
                                                                                         se=NA,
                                                                                         city=paste0(city_name),
                                                                                         edf=NA)});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_shannon_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/simple_gam_shannon_results_df.RDS")



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
    na.omit(.)
  
  mod <- mgcv::gam(log(abundance) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + 
                     s(season2, bs="cc", k=4) + urbanness, data=model_data)
  
  estimate <- tidy(mod, parametric=TRUE) %>%
    dplyr::filter(term=="urbanness") %>%
    .$estimate
  
  temp <- plot.gam(mod, page=1)
  
  plotting_data <- data.frame(estimate=estimate) %>%
    mutate(city=paste0(city_name)) %>%
    mutate(r_squared=summary(mod)$r.sq)
  
  return(plotting_data)
  
}

FaultTolerant_gam_abundance_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam_abundance(city_name);}, error=function(e) {ret <<- data.frame(x=NA,
                                                                                           y=NA,
                                                                                           se=NA,
                                                                                           city=paste0(city_name),
                                                                                           edf=NA)});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_abundance_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/simple_gam_abundance_results_df.RDS")


#########################################
#########################################
##### Now for total abundance ########
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
    na.omit(.)
  
  mod <- mgcv::gam(log(PD) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) + 
                     s(season2, bs="cc", k=4) + urbanness, data=model_data)
  
  estimate <- tidy(mod, parametric=TRUE) %>%
    dplyr::filter(term=="urbanness") %>%
    .$estimate
  
  temp <- plot.gam(mod, page=1)
  
  plotting_data <- data.frame(estimate=estimate) %>%
    mutate(city=paste0(city_name)) %>%
    mutate(r_squared=summary(mod)$r.sq)
  
  return(plotting_data)
  
}

FaultTolerant_gam_phylo_function <- function(city_name) {
  
  tryCatch({ret <- apply_gam_phylo(city_name);}, error=function(e) {ret <<- data.frame(x=NA,
                                                                                       y=NA,
                                                                                       se=NA,
                                                                                       city=paste0(city_name),
                                                                                       edf=NA)});
  ret
  
}


list <- unique(df$city)


results_list <- lapply(list, function(x) {FaultTolerant_gam_phylo_function(x)})

results_df <- bind_rows(results_list)

saveRDS(results_df, file="Data/simple_gam_phylo_results_df.RDS")
