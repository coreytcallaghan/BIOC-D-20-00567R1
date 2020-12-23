## Script to model total species richness
## in a city
## for all 1581 cities (all with > 50 lists)
## packages
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
library(RColorBrewer)

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

## Look at relationship between total richness
## and city size
df2 <- df %>%
  group_by(city) %>%
  summarise(mean_richness=mean(species_richness),
            sd_richness=sd(species_richness)) %>%
  inner_join(., city_specific_df, by="city")


## investigate species richness area relationship
## within the random polygons
## first get rid of any sites with < 50 checklists
random_polys_area <- random_polys %>%
  dplyr::filter(total_lists >= 50) %>%
  dplyr::select(total_richness, area.m, total_lists, lat, lng) %>%
  mutate(Analysis="Random")

## make plot of city area and species richness
ggplot(df2, aes(x=(ALAND10/1e6), y=total_richness))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total species richness")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/total_richness_vs_city_area.png", 
       width=5, height=3.7, units="in")


## make plot of mean species richness on all checklists
## in a city and the total species richness in that city
## this shows that, to some degree, the checklist level represents the
## total level of richness
## which provides some credence for looking at a checklist-level approach
## for the majority of the analysis
ggplot(df2, aes(x=mean_richness, y=total_richness))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Total species richness")+
  xlab("Mean species richness")

ggsave(filename="Figures/total_richness_vs_mean_checklist_richness.png", 
       width=5, height=3.7, units="in")

## make plot between total richness and total eBird lists
## This plot shows the obvious relationship between
## sampling and species richness
## and demonstrates the need to use 
## the number of lists as an offset in downstream models
ggplot(df2, aes(x=total_lists, y=total_richness))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total species richness")+
  xlab("Total number of eBird lists (log)")

ggsave(filename="Figures/total_richness_vs_total_ebird_lists.png", 
       width=5, height=3.7, units="in")


# Also make a plot of
# total eBird lists versus city area
ggplot(df2, aes(x=(ALAND10/1e6), y=total_lists))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total number of eBird lists (log)")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/city_area_vs_total_ebird_lists.png", 
       width=5, height=3.7, units="in")

# plot the distribution of checklists
# among cities
# and summarize this information for the paper
ggplot(df2, aes(x=total_lists))+
  geom_histogram(color="black", fill="blue", bins=100)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Count")+
  xlab("Total number of eBird lists (log)")

ggsave(filename="Figures/histogram_total_ebird_lists_city.png", 
       width=5, height=3.7, units="in")

mean(df2$total_lists)
sd(df2$total_lists)
min(df2$total_lists)
max(df2$total_lists)
median(df2$total_lists)

# now model the relationship between city size and total richness
# first run a linear model of the relationship
# and then get the residuals and model that with predictor variables
# run linear model for the relationship
# can include this in results of paper
#hist(df2$total_richness)
#lm_mod <- lm(total_richness~log((ALAND10/1000)), offset=(log(df2$total_lists)), data=df2)
#summary(lm_mod)

#hist(log(df2$total_richness))
#lm_mod.log <- lm(log(total_richness)~log((ALAND10/1000)), offset=(log(df2$total_lists)), data=df2)
#summary(lm_mod.log)

gam_mod <- mgcv::gam(total_richness~log(ALAND10/1e6)+s(lat, lng, bs="sos",m=2,k=100), 
                     weights=log(total_lists), family=poisson(), method="GCV.Cp", data=df2)
summary(gam_mod)
plot.gam(gam_mod, all.terms=TRUE, page=1)

# rerun the model without a smoother which will extract residuals from
# which will then be used as the residual spcies richness in later analyses
gam_mod.2 <- mgcv::gam(total_richness~log(ALAND10/1e6), weights=log(total_lists), family=poisson(), data=df2)
summary(gam_mod.2)
plot.gam(gam_mod.2, all.terms=TRUE, page=1)


viz_obj <- getViz(gam_mod)

plot(viz_obj, allTerms=TRUE, select=2)+
  l_points()+ 
  l_fitLine(linetype = "solid", color="blue")+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Adjusted species richness")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))
  
ggsave(filename="Figures/adjusted_species_richness_from_gam_vs_city_size.png", 
       width=5, height=3.7, units="in")

# re-run the same model, for the random polygon patches
dat <- random_polys_area %>%
  dplyr::filter(total_lists >= sample_size)

gam_mod <- mgcv::gam(total_richness~log(area.m/1e6)+s(lat, lng, bs="sos",m=2,k=100), 
                     weights=log(total_lists), family=poisson(),  method="GCV.Cp", data=random_polys_area)
summary(gam_mod)

viz_obj <- getViz(gam_mod)

plot(viz_obj, allTerms=TRUE, select=2)+
  l_points()+ 
  l_fitLine(linetype = "solid", color="blue")+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Adjusted species richness")+
  xlab(bquote('City area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/adjusted_species_richness_from_gam_vs_random_poly_size.png", 
       width=5, height=3.7, units="in")

# plot both city size and random area in one plot
df3 <- df2 %>%
  rename(area.m=ALAND10) %>%
  dplyr::select(total_richness, area.m, total_lists, lat, lng) %>%
  mutate(Analysis="Cities")

city_vs_random_comparison <- bind_rows(df3, random_polys_area)

ggplot(city_vs_random_comparison, aes(x=area.m/1e6, y=total_richness, color=Analysis))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total species richness")+
  xlab(bquote('Patch area ('* 'log('~km^2*'))'))+
  scale_color_brewer(palette = "Set1")+
  theme(legend.position=c(0.2, 0.8))

ggsave(filename="Figures/city_and_random_poly_total_richness.png", 
       width=5, height=3.7, units="in")


gam_mod <- mgcv::gam(total_richness~log(area.m/1e6)*Analysis+s(lat, lng, bs="sos",m=2,k=100), 
                     weights=log(total_lists), method="GCV.Cp", family=poisson(), city_vs_random_comparison)
summary(gam_mod)
plot.gam(gam_mod, all.terms=TRUE, page=1)

gam_mod_no_interaction <- mgcv::gam(total_richness~log(area.m/1e6)+Analysis+s(lat, lng, bs="sos",m=2,k=100), 
                                    weights=log(total_lists), family=poisson(), city_vs_random_comparison)
summary(gam_mod_no_interaction)
plot.gam(gam_mod_no_interaction, all.terms=TRUE, page=1)



# But we actually want to model the residuals to see how the most over-performing
# cities do compared with the under-performing cities
df2$resids <- resid(gam_mod.2)

# select variables of interest and response (resids)
# and rescale these variables for better understanding of model results
df3 <- df2 %>%
  dplyr::select(population, density, mean_EVI, tree_mean, 
                proportion_water, lat, lng, resids, distance.to.coast_km) %>%
  mutate(s_population=arm::rescale(log(population))) %>%
  mutate(s_density=arm::rescale(density)) %>%
  mutate(s_mean_EVI=arm::rescale(mean_EVI)) %>%
  mutate(s_tree_mean=arm::rescale(tree_mean)) %>%
  mutate(s_proportion_water=arm::rescale(log(proportion_water+0.001))) %>%
  mutate(s_distance.to.coast_km=arm::rescale(distance.to.coast_km))

# lets look at correlation among potential predictor variables
cor_object <- df3 %>%
  dplyr::select(s_mean_EVI, s_tree_mean, s_proportion_water, 
                s_distance.to.coast_km) %>%
  ungroup()

ggcorrplot(cor(na.omit(cor_object[1:4])), lab=TRUE)+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave(filename="Figures/total_richness_predictor_correlation.png", 
       width=5, height=3.7, units="in")


# it looks fine and not too much correlation
# among the predictors
# let's run a gam so we can include the
# lat lng as a smoother to account for spatial autocorrelation
hist(df3$resids)

resid_mod <- mgcv::gam(resids ~ s_distance.to.coast_km + s_mean_EVI + s_tree_mean +
                         s_tree_mean*s_proportion_water + s_proportion_water + 
                         s(lat, lng, bs="sos",m=2,k=100), data=df3)

summary(resid_mod)

resid_mod.2 <- mgcv::gam(resids ~ s_mean_EVI + s_tree_mean +
                         s_tree_mean*s_proportion_water + s_proportion_water + 
                         s(lat, lng, bs="sos",m=2,k=100), data=df3)

summary(resid_mod.2)

a <- ggplot(df3, aes(x=s_tree_mean, y=resids))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", size=6.5))+
  theme(axis.text.y=element_text(color="black", size=6.5))+
  theme(axis.title=element_text(color="black", size=8))+
  ylab("Residual species richness")+
  xlab("Standardized tree mean")

b <- ggplot(df3, aes(x=s_proportion_water, y=resids))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", size=6.5))+
  theme(axis.text.y=element_text(color="black", size=6.5))+
  theme(axis.title=element_text(color="black", size=8))+
  ylab("")+
  xlab("Standardized proportion water")

c <- ggplot(df3, aes(x=s_distance.to.coast_km, y=resids))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", size=6.5))+
  theme(axis.text.y=element_text(color="black", size=6.5))+
  theme(axis.title=element_text(color="black", size=8))+
  ylab("Residual species richness")+
  xlab("Standardized distance to coast")

d <- ggplot(df3, aes(x=s_mean_EVI, y=resids))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", size=6.5))+
  theme(axis.text.y=element_text(color="black", size=6.5))+
  theme(axis.title=element_text(color="black", size=8))+
  ylab("")+
  xlab("Standardized EVI")

# make plot of the four key variables
# with four panels
# showing the relationship
a + b + c + d + plot_layout(ncol=2)

ggsave(filename="Figures/predictor_plots_raw.png", 
       width=5, height=3.7, units="in")

## Make a figure summarizing the results of the gam model
plot_summary_dat <- tidy(resid_mod, parametric=TRUE) %>%
  mutate(lower_confint=confint(resid_mod)$`2.5%`) %>%
  mutate(upper_confint=confint(resid_mod)$`97.5%`) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(significance=case_when(
    p.value > 0.05 ~ "Not Significant",
    p.value < 0.05 ~ "Significant"
  )) %>%
  mutate(term=gsub("s_tree_mean:s_proportion_water", "Trees*water", .$term)) %>%
  mutate(term=gsub("s_tree_mean","Trees", .$term)) %>%
  mutate(term=gsub("s_proportion_water", "Water", .$term)) %>%
  mutate(term=gsub("s_mean_EVI", "EVI", .$term)) %>%
  mutate(term=gsub("s_distance.to.coast_km", "Distance to coast", .$term))

ggplot(plot_summary_dat, aes(x=term, y=estimate))+
  geom_hline(yintercept=0, color="red")+
  geom_point()+
  geom_errorbar(aes(ymin=lower_confint, ymax=upper_confint), width=0.2)+
  coord_flip()+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Standardized parameter estimate")+
  xlab("")

ggsave(filename="Figures/total_richness_mod_results.png", 
       width=5, height=3.7, units="in")




## make a plot of lat/lng for cities
## and for random polys as a reviewer was concerned about the
## potential latitudinal and longitudinal gradients of cities in species richness
## and that these were not accounted for in the random polygon assignment
brewer.pal(n=3, name="Set1")

ggplot()+
  geom_point(data=df2, aes(x=lng, y=lat), color="#E41A1C", alpha=0.6)+
  geom_point(data=random_polys_area, aes(x=lng, y=lat), color="#377EB8", alpha=0.6)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  xlab("Longitude")+
  ylab("Latitude")

ggsave(filename="Figures/coords_of_cities_and_random_polys.png", 
       width=5, height=3.7, units="in")

# make a plot of area
ggplot(random_polys_area, aes(x=area.m/1e6, y=total_richness))+
  geom_point(size=0.7)+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  scale_x_log10(labels=comma)+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  ylab("Total species richness")+
  xlab(bquote('Patch area ('* 'log('~km^2*'))'))

ggsave(filename="Figures/total_richness_vs_random_poly_area.png", 
       width=5, height=3.7, units="in")

lm_mod_random <- lm(total_richness~log((area.m/1e6)), weights=(random_polys_area$total_lists), data=random_polys_area)
summary(lm_mod_random)

lm_mod_random.log <- lm(log(total_richness)~log((area.m/1e6)), weights=(random_polys_area$total_lists), data=random_polys_area)
summary(lm_mod_random.log)


