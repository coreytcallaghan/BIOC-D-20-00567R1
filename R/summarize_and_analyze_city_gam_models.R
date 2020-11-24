# AN R script to perform analyses and make figures of
# the gam results which were run for each city 
# in the "apply_gam_to_each_city.R" script

# packages
library(dplyr)
library(ggplot2)
library(scales)
library(arm)
library(mgcv)
library(tidyr)
library(ggcorrplot)
library(corrplot)

source("R/global_functions.R")


# load data
richness <- readRDS("Data/gam_richness_results_df.RDS") %>%
  mutate(analysis="Species richness")
abundance <- readRDS("Data/gam_abundance_results_df.RDS") %>%
  mutate(analysis="Abundance")
shannon <- readRDS("Data/gam_shannon_results_df.RDS") %>%
  mutate(analysis="Shannon diversity")
phylo <- readRDS("Data/gam_phylo_results_df.RDS") %>%
  mutate(analysis="Phylogenetic diversity")


# city-specific data
load("Data for modelling/city_specific_data.RData")

analysis_df <- bind_rows(richness, abundance, shannon, phylo) %>%
  left_join(., city_specific_df, by="city")

# first see how many models failed to converge for each
# of the four analyses
# could be for a number of reasons
# but will include a blanket statement in the methods/results of paper
analysis_df %>%
  group_by(analysis, city) %>%
  summarise(N=n()) %>%
  dplyr::filter(N==1) %>%
  ungroup() %>%
  group_by(analysis) %>%
  summarise(N=n())


# Scale and center x and y
# for each group
# and then calculate the slope of each
analysis_df_scaled <- analysis_df %>%
  group_by(city, analysis) %>%
  mutate(x_s = scale_this(x)) %>%
  mutate(y_s = scale_this(y)) %>%
  mutate(y2_s=lead(y_s)) %>%
  mutate(x2_s=lead(x_s)) %>%
  mutate(delta_y_s=y2_s-y_s) %>%
  mutate(delta_x_s=x2_s-x_s) %>%
  mutate(slope_s=delta_y_s/delta_x_s) %>%
  group_by(city, analysis) %>%
  mutate(mean_slope_all_s=mean(slope_s, na.rm=TRUE)) %>%
  left_join(., summarise(slice(group_by(., city, analysis), 5:95), mean(slope_s))) %>%
  rename(mean_slope_90_s=`mean(slope_s)`)


# plot 20 random cities and their response to urbanization
# at the checklist level
random_cities <- analysis_df_scaled %>%
  replace_na(list(x=0)) %>%
  dplyr::filter(x != 0) %>%
  group_by(city) %>%
  sample_n_groups(5)
  

ggplot(random_cities, aes(x=x_s, y=y_s, color=analysis))+
  geom_line()+
  facet_wrap(city~analysis, scales="free", ncol=4)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", size=6))+
  theme(axis.text.y=element_text(color="black", size=6))+
  theme(axis.title=element_text(color="black"))+
  ylab("Smoothed response")+
  xlab("Urbanization level")+
  guides(color=guide_legend(title="   Analysis"))+
  theme(legend.position="bottom")+
  theme(strip.text.x = element_text(size = 4.5, colour = "black"))+
  guides(color=FALSE)
  

ggsave(filename="Figures/example_smooths_for_5_cities.png", 
       width=4.5, height=8, units="in")

#### This is helpful
#### but would be nice to look at each city separately
#### to really understand what is going on
#### going to write a for loop to loop through each city
#### make a plot and then save it
for (i in unique(analysis_df_scaled$city)) {
  
  dat <- analysis_df_scaled %>%
    dplyr::filter(city==i) %>%
    mutate(mean_slope_all_s=round(mean_slope_all_s, digits=2)) %>%
    mutate(mean_slope_90_s=round(mean_slope_90_s, digits=2)) %>%
    unite(facets, analysis, mean_slope_all_s, mean_slope_90_s, sep="_")
  
  ggplot(dat, aes(x=x_s, y=y_s, color=facets))+
    geom_line()+
    facet_wrap(~facets, scales="free", ncol=2)+
    theme_classic()+
    theme(panel.border = element_rect(color="black", fill=NA))+
    theme(axis.text.x=element_text(color="black", size=8))+
    theme(axis.text.y=element_text(color="black", size=8))+
    theme(axis.title=element_text(color="black"))+
    ylab("Smoothed response")+
    xlab("Urbanization level")+
    guides(color=guide_legend(title="   Analysis"))+
    theme(legend.position="bottom")+
    theme(strip.text.x = element_text(size = 6.5, colour = "black"))+
    guides(color=FALSE)+
    ggtitle(paste0(dat$city, ", city size: ", dat$ALAND10/1000, "km2"))
  
  ggsave(filename=paste0("Figures/City_smoothed_plot/", i, ".png"), 
         width=5, height=3.7, units="in")
  
}




### Try to plot ALL SMOOTHS for every city on a 
### single plot
### does not look good!
analysis_df_scaled %>%
  replace_na(list(x=0)) %>%
  dplyr::filter(x != 0) %>%
  ggplot(., aes(x=x_s, y=y_s, group=city))+
  geom_line()+
  facet_wrap(~analysis, scales="free_y")

#################################################
# investigate mean slope in relation to city size
slope_city <- analysis_df_scaled %>%
  replace_na(list(x=0)) %>%
  dplyr::filter(x != 0) %>%
  group_by(analysis, city) %>%
  summarise(mean_slope.all=mean(mean_slope_all, na.rm=TRUE),
            mean_slope.90=mean(mean_slope_90, na.rm=TRUE),
            r_squared=mean(r_squared, na.rm=TRUE),
            mean_slope.all_s=mean(mean_slope_all_s, na.rm=TRUE),
            mean_slope.90_s=mean(mean_slope_90_s, na.rm=TRUE)) %>%
  left_join(., city_specific_df, by="city")


### Make some summary plots
# number of cities for each analysis
table(slope_city$analysis)

# histogram of mean slope
# when calculating the slope using
# every point combination
ggplot(slope_city, aes(x=mean_slope.all_s))+
  geom_histogram(color="black", fill="blue", bins=100)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Count")+
  xlab("Mean slope of response to urbanization")+
  facet_wrap(~analysis)

ggsave(filename="Figures/histogram_of_all_slopes.png", 
       width=5, height=3.7, units="in")

## summarize this information for the paper
slope_city %>%
  group_by(analysis) %>%
  mutate(sign=case_when(
    mean_slope.all > 0 ~ "Positive",
    mean_slope.all < 0 ~ "Negative"
  )) %>%
  group_by(analysis, sign) %>%
  summarise(N=n())

# histogram of mean slope
# when calculating the slope using
# the middle points - getting rid of the 5 points
# on each end of the smooth curve
ggplot(slope_city, aes(x=mean_slope.90_s))+
  geom_histogram(color="black", fill="blue", bins=100)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Count")+
  xlab("Mean slope of response to urbanization")+
  facet_wrap(~analysis)

ggsave(filename="Figures/histogram_of_90_slopes.png", 
       width=5, height=3.7, units="in")

## Plot the relationship between slope and city size!
ggplot(slope_city, aes(x=(ALAND10/1000), y=mean_slope.all_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~analysis, scales="free")+
  scale_x_log10(labels=comma)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Mean slope")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/slope_vs_city_size_all.png", 
       width=5.6, height=3.7, units="in")

ggplot(slope_city, aes(x=(ALAND10/1000), y=mean_slope.90_s))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~analysis, scales="free")+
  scale_x_log10(labels=comma)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Mean slope")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/slope_vs_city_size_90.png", 
       width=5, height=3.7, units="in")

# Looks pretty crazy, and hard to see anything because quite a few points that are really noisy!
# Bimodal distribution with peaks at 1 and -1
# I'll first just try sorting by r_squared and only include
# models that had an r_squared of > .5 just to see if that gets rid of some of the noise
slope_city %>%
  dplyr::filter(r_squared > .5) %>%
  ggplot(., aes(x=(ALAND10/1000), y=mean_slope.all_s))+
  geom_point()+
  facet_wrap(~analysis, scales="free")+
  scale_x_log10(labels=comma)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Mean slope")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/slope_vs_city_size_trimmed.png", 
       width=5, height=3.7, units="in")



## Now lets model the slope and see what (if anything)
## predicts the slope of the response to urbanization
## first create dataframe
## and standardize predictors
mod_data_sr <- slope_city %>%
  dplyr::filter(analysis=="Species richness") %>%
  ungroup() %>%
  dplyr::select(population, density, mean_EVI, tree_mean, ALAND10,
                mean_slope.all_s, mean_slope.90_s, r_squared, total_lists, total_richness,
                proportion_water, lat, lng, distance.to.coast_km) %>%
  mutate(s_population=arm::rescale(log(population))) %>%
  mutate(s_density=arm::rescale(density)) %>%
  mutate(s_mean_EVI=arm::rescale(mean_EVI)) %>%
  mutate(s_tree_mean=arm::rescale(tree_mean)) %>%
  mutate(s_proportion_water=arm::rescale(log(proportion_water+0.001))) %>%
  mutate(s_distance.to.coast_km=arm::rescale(distance.to.coast_km)) %>%
  mutate(s_city_area=arm::rescale(log(ALAND10/1000)))

## Now check for correlation
cor_object <- mod_data_sr %>%
  dplyr::select(s_mean_EVI, s_tree_mean, s_proportion_water, s_density,
                s_distance.to.coast_km, s_city_area, s_population) %>%
  ungroup()

ggcorrplot(cor(na.omit(cor_object[1:7])))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave(filename="Figures/slope_model_predictor_correlation.png", 
       width=5, height=3.7, units="in")

## looks like population and city area are highly correlated! (Unsurprisingly)
## Will get rid of population and only include area
## repeat the above plot but without population
ggcorrplot(cor(na.omit(cor_object[1:6])))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave(filename="Figures/slope_model_predictor_correlation_2.png", 
       width=5, height=3.7, units="in")

# now run model with these predictors
# and a smooth for lat lng
slope_mod_sr <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                          s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                          weights=r_squared, data=mod_data_sr) 

summary(slope_mod_sr)

## This model doesn't work too well because
## of the bimodal distribution in data
## So lets try running a binomial model and a normal model
## normal model first
mod_data_sr_norm <- mod_data_sr %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != 1.00) %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != -1.00)

# check histogram
hist(mod_data_sr_norm$mean_slope.all_s)

# run normal model
slope_mod_sr_norm <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                            s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                          weights=r_squared, data=mod_data_sr_norm) 

summary(slope_mod_sr_norm)

## binomial model now
## where -1 is set to 0 and 1 is set to 1
mod_data_sr_binom <- mod_data_sr %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) == 1.00 | round(mean_slope.all_s, digits=2) == -1.00) %>%
  mutate(mean_slope.all_s = ifelse(round(mean_slope.all_s, digits=2) == 1.00, 1, 0)) %>%
  mutate(mean_slope.all_s = as.integer(as.numeric(mean_slope.all_s)))

# check histogram
hist(mod_data_sr_binom$mean_slope.all_s)

# run binom model
slope_mod_sr_binom <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                 s_proportion_water + s_city_area + s(lat, lng)  + s_density, 
                               weights=r_squared, family=binomial(), data=mod_data_sr_binom) 

summary(slope_mod_sr_binom)

## Try running a binom model now that
## has any that are > 0.5 as 'positive'
## and any < -0.5 as 'negative'
mod_data_sr_all_binom <- mod_data_sr %>%
  mutate(mean_slope.all_s = case_when(
    mean_slope.all_s > 0.5 ~ 1,
    mean_slope.all_s < -.05 ~ 0
  )) %>%
  dplyr::filter(mean_slope.all_s == 1 | mean_slope.all_s == 0)

slope_mod_sr_binom2 <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                  s_proportion_water + s_city_area + s(lat, lng)  + s_density, 
                                weights=r_squared, family=binomial(), data=mod_data_sr_all_binom) 

summary(slope_mod_sr_binom2)


### repeat the above but for abundance
mod_data_abund <- slope_city %>%
  dplyr::filter(analysis=="Abundance") %>%
  ungroup() %>%
  dplyr::select(population, density, mean_EVI, tree_mean, ALAND10,
                mean_slope.all_s, mean_slope.90_s, r_squared, total_lists, total_richness,
                proportion_water, lat, lng, distance.to.coast_km) %>%
  mutate(s_population=arm::rescale(log(population))) %>%
  mutate(s_density=arm::rescale(density)) %>%
  mutate(s_mean_EVI=arm::rescale(mean_EVI)) %>%
  mutate(s_tree_mean=arm::rescale(tree_mean)) %>%
  mutate(s_proportion_water=arm::rescale(log(proportion_water+0.001))) %>%
  mutate(s_distance.to.coast_km=arm::rescale(distance.to.coast_km)) %>%
  mutate(s_city_area=arm::rescale(log(ALAND10/1000)))

slope_mod_abund <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                               s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                             weights=r_squared, data=mod_data_abund) 


summary(slope_mod_abund)

## This model doesn't work too well because
## of the bimodal distribution in data
## So lets try running a binomial model and a normal model
## normal model first
mod_data_abund_norm <- mod_data_abund %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != 1.00) %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != -1.00)

# check histogram
hist(mod_data_abund_norm$mean_slope.all_s)

# run normal model
slope_mod_abund_norm <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                 s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                               weights=r_squared, data=mod_data_abund_norm) 

summary(slope_mod_abund_norm)

## binomial model now
## where -1 is set to 0 and 1 is set to 1
mod_data_abund_binom <- mod_data_abund %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) == 1.00 | round(mean_slope.all_s, digits=2) == -1.00) %>%
  mutate(mean_slope.all_s = ifelse(round(mean_slope.all_s, digits=2) == 1.00, 1, 0)) %>%
  mutate(mean_slope.all_s = as.integer(as.numeric(mean_slope.all_s)))

# check histogram
hist(mod_data_abund_binom$mean_slope.all_s)

# run binom model
slope_mod_abund_binom <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                  s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                weights=r_squared, family=binomial(), data=mod_data_abund_binom) 

summary(slope_mod_abund_binom)

## Try running a binom model now that
## has any that are > 0.5 as 'positive'
## and any < -0.5 as 'negative'
mod_data_abund_all_binom <- mod_data_abund %>%
  mutate(mean_slope.all_s = case_when(
    mean_slope.all_s > 0.5 ~ 1,
    mean_slope.all_s < -.05 ~ 0
  )) %>%
  dplyr::filter(mean_slope.all_s == 1 | mean_slope.all_s == 0)

slope_mod_abund_binom2 <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                   s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                 weights=r_squared, family=binomial(), data=mod_data_abund_all_binom) 

summary(slope_mod_abund_binom2)



### repeat the above but for shannon diversity
mod_data_shannon <- slope_city %>%
  dplyr::filter(analysis=="Shannon diversity") %>%
  ungroup() %>%
  dplyr::select(population, density, mean_EVI, tree_mean, ALAND10,
                mean_slope.all_s, mean_slope.90_s, r_squared, total_lists, total_richness,
                proportion_water, lat, lng, distance.to.coast_km) %>%
  mutate(s_population=arm::rescale(log(population))) %>%
  mutate(s_density=arm::rescale(density)) %>%
  mutate(s_mean_EVI=arm::rescale(mean_EVI)) %>%
  mutate(s_tree_mean=arm::rescale(tree_mean)) %>%
  mutate(s_proportion_water=arm::rescale(log(proportion_water+0.001))) %>%
  mutate(s_distance.to.coast_km=arm::rescale(distance.to.coast_km)) %>%
  mutate(s_city_area=arm::rescale(log(ALAND10/1000)))

slope_mod_shannon <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                               s_proportion_water + s_city_area + s(lat, lng), 
                             weights=r_squared, data=mod_data_shannon) 


summary(slope_mod_shannon)


## This model doesn't work too well because
## of the bimodal distribution in data
## So lets try running a binomial model and a normal model
## normal model first
mod_data_shannon_norm <- mod_data_shannon %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != 1.00) %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != -1.00)

# check histogram
hist(mod_data_shannon_norm$mean_slope.all_s)

# run normal model
slope_mod_shannon_norm <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                    s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                  weights=r_squared, data=mod_data_shannon_norm) 

summary(slope_mod_shannon_norm)

## binomial model now
## where -1 is set to 0 and 1 is set to 1
mod_data_shannon_binom <- mod_data_shannon %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) == 1.00 | round(mean_slope.all_s, digits=2) == -1.00) %>%
  mutate(mean_slope.all_s = ifelse(round(mean_slope.all_s, digits=2) == 1.00, 1, 0)) %>%
  mutate(mean_slope.all_s = as.integer(as.numeric(mean_slope.all_s)))

# check histogram
hist(mod_data_shannon_binom$mean_slope.all_s)

# run binom model
slope_mod_shannon_binom <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                     s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                   weights=r_squared, family=binomial(), data=mod_data_shannon_binom) 

summary(slope_mod_shannon_binom)

## Try running a binom model now that
## has any that are > 0.5 as 'positive'
## and any < -0.5 as 'negative'
mod_data_shannon_all_binom <- mod_data_shannon %>%
  mutate(mean_slope.all_s = case_when(
    mean_slope.all_s > 0.5 ~ 1,
    mean_slope.all_s < -.05 ~ 0
  )) %>%
  dplyr::filter(mean_slope.all_s == 1 | mean_slope.all_s == 0)

slope_mod_shannon_binom2 <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                      s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                    weights=r_squared, family=binomial(), data=mod_data_shannon_all_binom) 

summary(slope_mod_shannon_binom2)


### repeat the above but for phylogenetic diversity
mod_data_phylo <- slope_city %>%
  dplyr::filter(analysis=="Phylogenetic diversity") %>%
  ungroup() %>%
  dplyr::select(population, density, mean_EVI, tree_mean, ALAND10,
                mean_slope.all_s, mean_slope.90_s, r_squared, total_lists, total_richness,
                proportion_water, lat, lng, distance.to.coast_km) %>%
  mutate(s_population=arm::rescale(log(population))) %>%
  mutate(s_density=arm::rescale(density)) %>%
  mutate(s_mean_EVI=arm::rescale(mean_EVI)) %>%
  mutate(s_tree_mean=arm::rescale(tree_mean)) %>%
  mutate(s_proportion_water=arm::rescale(log(proportion_water+0.001))) %>%
  mutate(s_distance.to.coast_km=arm::rescale(distance.to.coast_km)) %>%
  mutate(s_city_area=arm::rescale(log(ALAND10/1000)))

slope_mod_phylo <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                 s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                               weights=r_squared, data=mod_data_phylo) 


summary(slope_mod_phylo)

## This model doesn't work too well because
## of the bimodal distribution in data
## So lets try running a binomial model and a normal model
## normal model first
mod_data_phylo_norm <- mod_data_phylo %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != 1.00) %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) != -1.00)

# check histogram
hist(mod_data_phylo_norm$mean_slope.all_s)

# run normal model
slope_mod_phylo_norm <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                    s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                  weights=r_squared, data=mod_data_phylo_norm) 

summary(slope_mod_phylo_norm)

## binomial model now
## where -1 is set to 0 and 1 is set to 1
mod_data_phylo_binom <- mod_data_phylo %>%
  dplyr::filter(round(mean_slope.all_s, digits=2) == 1.00 | round(mean_slope.all_s, digits=2) == -1.00) %>%
  mutate(mean_slope.all_s = ifelse(round(mean_slope.all_s, digits=2) == 1.00, 1, 0)) %>%
  mutate(mean_slope.all_s = as.integer(as.numeric(mean_slope.all_s)))

# check histogram
hist(mod_data_phylo_binom$mean_slope.all_s)

# run binom model
slope_mod_phylo_binom <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                     s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                   weights=r_squared, family=binomial(), data=mod_data_phylo_binom) 

summary(slope_mod_phylo_binom)

## Try running a binom model now that
## has any that are > 0.5 as 'positive'
## and any < -0.5 as 'negative'
mod_data_phylo_all_binom <- mod_data_phylo %>%
  mutate(mean_slope.all_s = case_when(
    mean_slope.all_s > 0.5 ~ 1,
    mean_slope.all_s < -.05 ~ 0
  )) %>%
  dplyr::filter(mean_slope.all_s == 1 | mean_slope.all_s == 0)

slope_mod_phylo_binom2 <- mgcv::gam(mean_slope.all_s ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                                      s_proportion_water + s_city_area + s(lat, lng) + s_density, 
                                    weights=r_squared, family=binomial(), data=mod_data_phylo_all_binom) 

summary(slope_mod_phylo_binom2)






#########################################################################################
#########################################################################################
########### Now look at a more simpler approach using a linear term in the models #######
#########################################################################################
#########################################################################################
# load data
richness_simple <- readRDS("Data/simple_gam_richness_results_df.RDS") %>%
  mutate(analysis="Species richness")
abundance_simple <- readRDS("Data/simple_gam_abundance_results_df.RDS") %>%
  mutate(analysis="Abundance")
shannon_simple <- readRDS("Data/simple_gam_shannon_results_df.RDS") %>%
  mutate(analysis="Shannon diversity")
phylo_simple <- readRDS("Data/simple_gam_phylo_results_df.RDS") %>%
  mutate(analysis="Phylogenetic diversity")

analysis_df2 <- bind_rows(richness_simple, abundance_simple, 
                          shannon_simple, phylo_simple) %>%
  left_join(., city_specific_df, by="city")

# histogram of the estimates
# for each of the analyses
ggplot(analysis_df2, aes(x=estimate))+
  geom_histogram(color="black", fill="blue", bins=100)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Count")+
  xlab("Estimate for urbanness term")+
  facet_wrap(~analysis)

ggsave(filename="Figures/histogram_of_urbanness_estimates.png", 
       width=5, height=3.7, units="in")


## plot the relationship between approach #1 and approach #2
analysis_df2 %>%
  dplyr::select(estimate, city, analysis) %>%
  inner_join(., slope_city, by=c("city", "analysis")) %>%
  ggplot(., aes(x=mean_slope.all, y=estimate))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~analysis, scales="free")
  


## Plot the relationship between estimate and city size!
ggplot(analysis_df2, aes(x=(ALAND10/1000), y=estimate))+
  geom_point()+
  facet_wrap(~analysis, scales="free")+
  scale_x_log10(labels=comma)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", size=6.5, hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Estimate")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/estimate_vs_city_size_all.png", 
       width=5, height=3.7, units="in")


