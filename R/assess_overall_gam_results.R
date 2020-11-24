## This script is to read in the four 'overall' models
## fitted with city as a random effect
## and make a plot showing the overall impact of urbanization level
## on biodiversity

# packages
library(mgcv)
library(dplyr)
library(ggplot2)


# read in richness first
mod_sr <- readRDS("Data/species_richness_overall_gam.RDS")


## now look at summary
summary(mod_sr)

## extract stuff for better plotting
temp_df <- plot.gam(mod_sr, page=1)
plotting_data_sr <- data.frame(x=temp_df[[4]]$x,
                            y=temp_df[[4]]$fit,
                            se=temp_df[[4]]$se) %>%
  mutate(Analysis="Species richness")


# same thing for abundance
mod_abund <- readRDS("Data/abundance_overall_gam.RDS")


## now look at summary
summary(mod_abund)

## extract stuff for better plotting
temp_df <- plot.gam(mod_abund, page=1)
plotting_data_abund <- data.frame(x=temp_df[[4]]$x,
                               y=temp_df[[4]]$fit,
                               se=temp_df[[4]]$se) %>%
  mutate(Analysis="Abundance")


# same thing for shannon diversity
mod_shannon <- readRDS("Data/shannon_diversity_overall_gam.RDS")


## now look at summary
summary(mod_shannon)

## extract stuff for better plotting
temp_df <- plot.gam(mod_shannon, page=1)
plotting_data_shannon <- data.frame(x=temp_df[[4]]$x,
                                  y=temp_df[[4]]$fit,
                                  se=temp_df[[4]]$se) %>%
  mutate(Analysis="Shannon diversity")

# same thing for phylo diversity
mod_phylo <- readRDS("Data/phylo_diversity_overall_gam.RDS")


## now look at summary
summary(mod_phylo)

## extract stuff for better plotting
temp_df <- plot.gam(mod_phylo, page=1)
plotting_data_phylo <- data.frame(x=temp_df[[4]]$x,
                                  y=temp_df[[4]]$fit,
                                  se=temp_df[[4]]$se) %>%
  mutate(Analysis="Phylogenetic diversity")




combined_data_plot <- bind_rows(plotting_data_sr,
                                plotting_data_abund,
                                plotting_data_shannon,
                                plotting_data_phylo)
ggplot(combined_data_plot)+
  geom_line(aes(x=x, y=y, color=Analysis))+
  geom_line(aes(x=x, y=y+se), linetype="dashed")+
  geom_line(aes(x=x, y=y-se), linetype="dashed")+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Smoothed biodiversity response")+
  xlab("Urbanization level (VIIRS night-time lights)")+
  facet_wrap(~Analysis, ncol=2)+
  guides(color=FALSE)


ggsave(filename="Figures/overall_gam_results_smoothed.png", 
       width=4.5, height=4, units="in")


### Now look at huge model results with parametric terms
### this allows us to see which standardized predictors
### are most important

# Species richness first
mod_sr_para <- readRDS("Data/species_richness_overall_gam_parametric_terms.RDS")

summary(mod_sr_para)

# shannon diversity
mod_shannon_para <- readRDS("Data/shannon_diversity_overall_gam_parametric_terms.RDS")

summary(mod_shannon_para)

# abundance
mod_abundance_para <- readRDS("Data/abundance_overall_gam_parametric_terms.RDS")

summary(mod_abundance_para)

# Phylogenetic diversity
mod_phylo_para <- readRDS("Data/phylo_diversity_overall_gam_parametric_terms.RDS")

summary(mod_phylo_para)







