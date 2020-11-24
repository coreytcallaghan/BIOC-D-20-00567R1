## This script repeats the main components of the analysis
## from the script: "total_species_richness_analysis.R"
## but for differing cutoffs of sample size

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(scales)
library(mgcv)
library(ggcorrplot)
library(corrplot)
library(arm)
library(mgcViz)
library(tibble)
library(mgcv.helper)
library(patchwork)

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

# read in a dataset of random polygons
random_polys <- readRDS("Data/random_polygon_richness_summary.RDS")

# read in the coordinates for each of the random polys
poly_coords <- readRDS("Data/random_poly_coords.RDS") %>%
  rename(lng=X) %>%
  rename(lat=Y)

# join polygon df with coords
random_polys <- random_polys %>%
  left_join(., poly_coords, by="polygon")

random_polys_area <- random_polys %>%
  dplyr::filter(total_lists >= 50) %>%
  dplyr::select(total_richness, area.m, total_lists, lat, lng) %>%
  mutate(Analysis="Random")

## Look at relationship between total richness
## and city size
df2 <- df %>%
  group_by(city) %>%
  summarise(mean_richness=mean(species_richness),
            sd_richness=sd(species_richness)) %>%
  inner_join(., city_specific_df, by="city")

# put both together
df3 <- df2 %>%
  rename(area.m=ALAND10) %>%
  dplyr::select(total_richness, area.m, total_lists, lat, lng) %>%
  mutate(Analysis="Cities")

city_vs_random_comparison <- bind_rows(df3, random_polys_area)


## make a plot of area versus total lists
ggplot(city_vs_random_comparison, aes(x=area.m/1000, y=total_lists, color=Analysis))+
  geom_point()+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  ylab("Total number of eBird lists (log)")+
  xlab(bquote('Patch area ('* 'log('~km^2*'))'))



# run a simple linear model for the city-size only
# relationship at the differing levels of sample size
gam_results_function_cities <- function(sample_size){
  
  dat <- df2 %>%
    dplyr::filter(total_lists >= sample_size)
  
  gam_mod <- mgcv::gam(total_richness~log(ALAND10/1000)+s(lat, lng, bs="sos",m=2,k=100), weights=log(total_lists), family=poisson(), data=dat)
  summary(gam_mod)
  
   N <- nrow(dat)
  
  results <- as.data.frame(summary(gam_mod)$p.table) %>%
    rownames_to_column(var="Term") %>%
    mutate(Cutoff=sample_size) %>%
    mutate(Fit=summary(gam_mod)$dev.expl) %>%
    mutate(N=N) %>%
    dplyr::filter(Term != "(Intercept)")
  
}

cutoff_size_vector <- seq(50, 1500, by=50)

results_list <- lapply(cutoff_size_vector, function(x)gam_results_function_cities(x))

results_df_cities <- bind_rows(results_list)


ggplot(results_df_cities, aes(x=Cutoff, y=N))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  xlab("Cutoff sample size")+
  ylab("Number of cities")



ggplot(results_df_cities, aes(x=Cutoff, y=Estimate))+
  geom_point()+
  theme_classic()+
  geom_errorbar(aes(ymax=Estimate+`Std. Error`, ymin=Estimate-`Std. Error`))+
  theme(axis.text=element_text(color="black"))+
  xlab("Cutoff sample size")+
  ylab("Slope estimate")+
  ggtitle("City analysis")



# now do the same thing, but just for random polygons
gam_results_function_random <- function(sample_size){
  
  dat <- random_polys_area %>%
    dplyr::filter(total_lists >= sample_size)
  
  gam_mod <- mgcv::gam(total_richness~log(area.m/1000)+s(lat, lng, bs="sos",m=2,k=100), weights=log(total_lists), family=poisson(), data=dat)
  summary(gam_mod)
  
  N <- nrow(dat)
  
  results <- as.data.frame(summary(gam_mod)$p.table) %>%
    rownames_to_column(var="Term") %>%
    mutate(Cutoff=sample_size) %>%
    mutate(Fit=summary(gam_mod)$dev.expl) %>%
    mutate(N=N) %>%
    dplyr::filter(Term != "(Intercept)")
  
}

cutoff_size_vector <- seq(50, 1500, by=50)

results_list <- lapply(cutoff_size_vector, function(x)gam_results_function_random(x))

results_df_random <- bind_rows(results_list)


ggplot(results_df_random, aes(x=Cutoff, y=Estimate))+
  geom_point()+
  theme_classic()+
  geom_errorbar(aes(ymax=Estimate+`Std. Error`, ymin=Estimate-`Std. Error`))+
  theme(axis.text=element_text(color="black"))+
  xlab("Cutoff sample size")+
  ylab("Slope estimate")+
  ggtitle("Random polygon analysis")



# now re-run the main model, which compares the slopes between the two
# 'analysis types' as a function of cutoff size
gam_results_function_both <- function(sample_size){
  
  dat <- city_vs_random_comparison %>%
    dplyr::filter(total_lists >= sample_size)

  gam_mod <- mgcv::gam(total_richness~log(area.m/1000)*Analysis+s(lat, lng, bs="sos",m=2,k=100), weights=log(total_lists), family=poisson(), data=dat)
  summary(gam_mod)
  
  
  N <- nrow(dat)
  
  results <- as.data.frame(summary(gam_mod)$p.table) %>%
    rownames_to_column(var="Term") %>%
    mutate(Cutoff=sample_size) %>%
    mutate(Fit=summary(gam_mod)$dev.expl) %>%
    mutate(N=N) %>%
    dplyr::filter(Term != "(Intercept)")
  
}



cutoff_size_vector <- seq(50, 1500, by=50)

results_list <- lapply(cutoff_size_vector, function(x)gam_results_function_both(x))

results_df_both <- bind_rows(results_list) 

ggplot(results_df_both %>%
         dplyr::filter(Term == "log(area.m/1000):AnalysisRandom"),
       aes(x=Cutoff, y=Estimate))+
  geom_point()+
  theme_classic()+
  geom_errorbar(aes(ymax=Estimate+`Std. Error`, ymin=Estimate-`Std. Error`))+
  theme(axis.text=element_text(color="black"))+
  xlab("Cutoff sample size")+
  ylab("Slope interaction")+
  ggtitle("Comparing random to cities (with weights)")



# now make key plot many times
# based on differing sample sizes
split_dat_function <- function(sample_size) {
  
  dat <- city_vs_random_comparison %>%
    dplyr::filter(total_lists >= sample_size) %>%
    mutate(Cutoff=sample_size)
  
  return(dat)
  
}

cutoff_size_vector <- seq(50, 1500, by=50)
split_dat_list <- lapply(cutoff_size_vector, function(x)split_dat_function(x))
split_dat_df <- bind_rows(split_dat_list)


ggplot(split_dat_df, aes(x=area.m/1000, y=total_richness, color=Analysis))+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  facet_wrap(~Cutoff)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  theme(axis.text.x=element_blank())+
  ylab("Total species richness")+
  xlab(bquote('Patch area ('* 'log('~km^2*'))'))










