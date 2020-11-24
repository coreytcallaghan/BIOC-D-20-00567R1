## This will repeat the overall analysis
## for each of the seasons
## but with a limited subset of cities

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


# run a gam model for each season
# Summer
summer_cities <- df2 %>%
  dplyr::select(Summer_richness, ALAND10, Summer_lists, lat, lng) %>%
  dplyr::filter(Summer_lists >= 50)

N <- nrow(summer_cities)

gam_mod <- mgcv::gam(Summer_richness~log(ALAND10/1000)+s(lat, lng, bs="sos",m=2,k=100), weights=log(Summer_lists), family=poisson(), data=summer_cities)
plot.gam(gam_mod, all.terms=TRUE, page=1)

summary(gam_mod)

summer_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Cutoff=sample_size) %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  mutate(N=N) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Summer")

# spring
spring_cities <- df2 %>%
  dplyr::select(Spring_richness, ALAND10, Spring_lists, lat, lng) %>%
  dplyr::filter(Spring_lists >=50)

N <- nrow(spring_cities)

gam_mod <- mgcv::gam(Spring_richness~log(ALAND10/1000)+s(lat, lng, bs="sos",m=2,k=100), weights=log(Spring_lists), family=poisson(), data=spring_cities)
plot.gam(gam_mod, all.terms=TRUE, page=1)

summary(gam_mod)


spring_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Cutoff=sample_size) %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  mutate(N=N) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Spring")


# Fall
autumn_cities <- df2 %>%
  dplyr::select(Autumn_richness, ALAND10, Autumn_lists, lat, lng) %>%
  dplyr::filter(Autumn_lists >=50)

N <- nrow(autumn_cities)

gam_mod <- mgcv::gam(Autumn_richness~log(ALAND10/1000)+s(lat, lng, bs="sos",m=2,k=100), weights=log(Autumn_lists), family=poisson(), data=autumn_cities)
plot.gam(gam_mod, all.terms=TRUE, page=1)

summary(gam_mod)


autumn_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Cutoff=sample_size) %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  mutate(N=N) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Autumn")


# Winter
winter_cities <- df2 %>%
  dplyr::select(Winter_richness, ALAND10, Winter_lists, lat, lng) %>%
  dplyr::filter(Winter_lists >=50)

N <- nrow(winter_cities)

gam_mod <- mgcv::gam(Winter_richness~log(ALAND10/1000)+s(lat, lng, bs="sos",m=2,k=100), weights=log(Winter_lists), family=poisson(), data=winter_cities)
plot.gam(gam_mod, all.terms=TRUE, page=1)

summary(gam_mod)


winter_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Cutoff=sample_size) %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  mutate(N=N) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Winter")


results_combined <- bind_rows(summer_results, winter_results, spring_results, autumn_results) 



temp_summer <- summer_cities %>%
  rename(richness=Summer_richness) %>%
  rename(list=Summer_lists) %>%
  mutate(Season="Summer")
temp_winter <- winter_cities %>%
  rename(richness=Winter_richness) %>%
  rename(list=Winter_lists) %>%
  mutate(Season="Winter")
temp_spring <- spring_cities %>%
  rename(richness=Spring_richness) %>%
  rename(list=Spring_lists) %>%
  mutate(Season="Spring")
temp_autumn <-autumn_cities %>%
  rename(richness=Autumn_richness) %>%
  rename(list=Autumn_lists) %>%
  mutate(Season="Autumn")

combined_dat_cities <- bind_rows(temp_summer, temp_winter,
                          temp_spring, temp_autumn) %>%
  mutate(Analysis="Cities") %>%
  rename(area.m=ALAND10)

ggplot(combined_dat_cities, aes(x=area.m/1000, y=richness))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  facet_wrap(~Season)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total species richness")+
  xlab(bquote('Patch area ('* 'log('~km^2*'))'))


random_polys_area <- random_polys %>%
  dplyr::filter(total_lists >= 50)


summer_random <- random_polys_area %>%
  dplyr::select(Summer_richness, area.m, Summer_lists, lat, lng) %>%
  mutate(Analysis="Random") %>%
  dplyr::filter(Summer_lists >= 50) %>%
  rename(richness=Summer_richness) %>%
  rename(list=Summer_lists) %>%
  mutate(Season="Summer")

spring_random <- random_polys_area %>%
  dplyr::select(Spring_richness, area.m, Spring_lists, lat, lng) %>%
  mutate(Analysis="Random") %>%
  dplyr::filter(Spring_lists >= 50) %>%
  rename(richness=Spring_richness) %>%
  rename(list=Spring_lists) %>%
  mutate(Season="Spring")

winter_random <- random_polys_area %>%
  dplyr::select(Winter_richness, area.m, Winter_lists, lat, lng) %>%
  mutate(Analysis="Random") %>%
  dplyr::filter(Winter_lists >= 50) %>%
  rename(richness=Winter_richness) %>%
  rename(list=Winter_lists) %>%
  mutate(Season="Winter")

autumn_random <- random_polys_area %>%
  dplyr::select(Autumn_richness, area.m, Autumn_lists, lat, lng) %>%
  mutate(Analysis="Random") %>%
  dplyr::filter(Autumn_lists >= 50) %>%
  rename(richness=Autumn_richness) %>%
  rename(list=Autumn_lists) %>%
  mutate(Season="Autumn")

combined_dat_random <- bind_rows(summer_random, spring_random,
                                 winter_random, autumn_random) %>%
  dplyr::select(1:5, 7, 6)


combined_dat_both <- bind_rows(combined_dat_cities,
                               combined_dat_random)  

  
ggplot(combined_dat_both, aes(x=area.m/1000, y=richness, color=Analysis))+
  geom_point(size=0.3)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  facet_wrap(~Season, scales="free")+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total species richness")+
  xlab(bquote('Patch area ('* 'log('~km^2*'))'))  
  



# Summer both
summer_model <- combined_dat_both %>%
  dplyr::filter(Season == "Summer")

gam_mod <- mgcv::gam(richness~log(area.m)*Analysis+s(lat, lng, bs="sos",m=2,k=100), weights=log(list), family=poisson(), data=summer_model)

summary(gam_mod)


summer_both_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Summer")


# Autumn both
autumn_model <- combined_dat_both %>%
  dplyr::filter(Season == "Autumn")

gam_mod <- mgcv::gam(richness~log(area.m/1000)*Analysis+s(lat, lng, bs="sos",m=2,k=100), weights=log(list), family=poisson(), data=autumn_model)

summary(gam_mod)


autumn_both_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Autumn")


# Winter both
winter_model <- combined_dat_both %>%
  dplyr::filter(Season == "Winter")

gam_mod <- mgcv::gam(richness~log(area.m/1000)*Analysis+s(lat, lng, bs="sos",m=2,k=100), weights=log(list), family=poisson(), data=winter_model)

summary(gam_mod)


winter_both_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Winter")


# Spring both
spring_model <- combined_dat_both %>%
  dplyr::filter(Season == "Spring")

gam_mod <- mgcv::gam(richness~log(area.m/1000)*Analysis+s(lat, lng, bs="sos",m=2,k=100), weights=log(list), family=poisson(), data=spring_model)

summary(gam_mod)

spring_both_results <- as.data.frame(summary(gam_mod)$p.table) %>%
  rownames_to_column(var="Term") %>%
  mutate(Fit=summary(gam_mod)$dev.expl) %>%
  dplyr::filter(Term != "(Intercept)") %>%
  mutate(Season="Spring")
  
interaction_intra_results <- bind_rows(summer_both_results, winter_both_results, spring_both_results, autumn_both_results)


  