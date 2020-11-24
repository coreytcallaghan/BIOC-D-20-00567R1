# This script is to run four 'overall'
# gam models, using 'bam'
# one for each response variable
# but fit with the same model for each
# treating city as a random effect
# export the results, but they are very large!
# so I just store them on my local machine!

## packages
library(dplyr)
library(tidyr)
library(mgcv)
library(ggplot2)
library(broom)
library(purrr)
library(readr)

source("R/global_functions.R")

## read in data
# response variables
load("Data for modelling/response_variables.RData")

# checklist-specific predictors
load("Data for modelling/checklist_specific_variables.RData")

# city-specific data
load("Data for modelling/city_specific_data.RData")

# read in checklist-level predictors
checklist_predictors <- readRDS("Data/checklist_predictors.RDS") %>%
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

# couple tweaks for modelling
model_data <- df %>%
  mutate(season2 = case_when(season=="Autumn" ~ 1,
                             season=="Winter" ~ 2,
                             season=="Spring" ~ 3,
                             season=="Summer" ~ 4)) %>%
  mutate(city = as.factor(as.character(city))) 

################################################
################################################
#### First a large gam for species richness ####
################################################
################################################

hist(df$species_richness)
hist(log(df$species_richness))

# just for species richness
mod_sr <- mgcv::bam(species_richness ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                      s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + s(urbanness) + s(season2, bs="cc", k=4) +
                      s(city, bs="re"), family=poisson(), data=model_data)


saveRDS(mod_sr, file="Data/species_richness_overall_gam.RDS")


################################################
################################################
#### Then a large gam for shannon diversity ####
################################################
################################################

hist(df$shannon_diversity)
hist(log(df$shannon_diversity))

# just for shannon diversity
mod_shannon <- mgcv::bam(shannon_diversity ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                      s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + s(urbanness) + s(season2, bs="cc", k=4) +
                      s(city, bs="re"), data=model_data)


saveRDS(mod_shannon, file="Data/shannon_diversity_overall_gam.RDS")


################################################
################################################
#### Then a large gam for total abundance ######
################################################
################################################

# just for total abundance
hist(df$abundance)
hist(log(df$abundance))
mod_abund <- mgcv::bam(log(abundance) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                      s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + s(urbanness) + s(season2, bs="cc", k=4) +
                      s(city, bs="re"), data=model_data)

saveRDS(mod_abund, file="Data/abundance_overall_gam.RDS")



################################################
################################################
# Then a large gam for phylogenetic diversity ##
################################################
################################################

# just for phylogenetic diversity
hist(df$PD)
hist(log(df$PD))
mod_phylo <- mgcv::bam(log(PD) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                         s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + s(urbanness) + s(season2, bs="cc", k=4) +
                         s(city, bs="re"), data=model_data)


saveRDS(mod_phylo, file="Data/phylo_diversity_overall_gam.RDS")



## Now repeat the above models, but run them with parametric terms
## for each checklist
## first scale the data so that effect sizes are comparable
model_data2 <- model_data %>%
  mutate_each(list(scale), urbanness, proportion_water.list, tree_mean.list, mean_EVI.list)


# just for species richness
mod_sr_parametric <- mgcv::bam(species_richness ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                      s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + urbanness + proportion_water.list +
                        tree_mean.list + mean_EVI.list + s(season2, bs="cc", k=4) +
                      s(city, bs="re"), family=poisson(), data=model_data2)


saveRDS(mod_sr_parametric, file="Data/species_richness_overall_gam_parametric_terms.RDS")

# species diversity
mod_shannon_parametric <- mgcv::bam(shannon_diversity ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                                 s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + urbanness + proportion_water.list +
                                 tree_mean.list + mean_EVI.list + s(season2, bs="cc", k=4) +
                                 s(city, bs="re"), data=model_data2)


saveRDS(mod_shannon_parametric, file="Data/shannon_diversity_overall_gam_parametric_terms.RDS")

# abundance
mod_abund_parametric <- mgcv::bam(log(abundance) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                                 s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + urbanness + proportion_water.list +
                                 tree_mean.list + mean_EVI.list + s(season2, bs="cc", k=4) +
                                 s(city, bs="re"), data=model_data2)


saveRDS(mod_abund_parametric, file="Data/abundance_overall_gam_parametric_terms.RDS")

# phylogenetic diversity
mod_phylo_parametric <- mgcv::bam(log(PD) ~ s(DURATION_MINUTES) + s(EFFORT_DISTANCE_KM) +
                                 s(LATITUDE, LONGITUDE, bs="sos",m=2,k=100) + urbanness + proportion_water.list +
                                 tree_mean.list + mean_EVI.list + s(season2, bs="cc", k=4) +
                                 s(city, bs="re"), data=model_data2)


saveRDS(mod_phylo_parametric, file="Data/phylo_diversity_overall_gam_parametric_terms.RDS")



