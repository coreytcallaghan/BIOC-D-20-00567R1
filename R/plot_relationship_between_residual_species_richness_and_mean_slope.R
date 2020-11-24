### plot the residuals of the total species richness
### against the slope of the line

## Packages
library(dplyr)
library(ggplot2)

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


## Look at relationship between total richness
## and city size
df2 <- df %>%
  group_by(city) %>%
  summarise(mean_richness=mean(species_richness),
            sd_richness=sd(species_richness)) %>%
  inner_join(., city_specific_df, by="city")

lm_mod <- lm(total_richness~log((ALAND10/1000)), weights=(df2$total_lists), data=df2)
summary(lm_mod)

df2$resids <- resid(lm_mod)


#### pull in the city-gam model results
#### and then summarize the slopes
# load data
richness <- readRDS("Data/gam_richness_results_df.RDS") %>%
  mutate(analysis="Species richness")
abundance <- readRDS("Data/gam_abundance_results_df.RDS") %>%
  mutate(analysis="Abundance")
shannon <- readRDS("Data/gam_shannon_results_df.RDS") %>%
  mutate(analysis="Shannon diversity")
phylo <- readRDS("Data/gam_phylo_results_df.RDS") %>%
  mutate(analysis="Phylogenetic diversity")


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



### combine the two datasets and then plot the results
slope_city %>%
  left_join(., dplyr::select(df2, city, resids), by="city") %>%
  ggplot(., aes(x=mean_slope.all_s, y=resids))+
  geom_point()+
  facet_wrap(~analysis, scales="free")+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black", hjust=1))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  theme(axis.ticks.x=element_blank())+
  xlab("Mean slope")+
  ylab("Residual species richness")

ggsave(filename="Figures/slope_vs_residual_species_richness.png", 
       width=5, height=3.7, units="in")












