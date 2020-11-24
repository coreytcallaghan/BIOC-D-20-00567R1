# AN R script to
# summarize the results of the
# parametric model terms
# which show the relationship between water, tree, urbanness, and evi
# at a checklist level
# based on the script: "assess_checklist_level_predictors_parametric.R"


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
richness <- readRDS("Data/gam_richness_results_parametric_model_df.RDS") %>%
  mutate(analysis="Species richness")
abundance <- readRDS("Data/gam_abundance_results_parametric_model_df.RDS") %>%
  mutate(analysis="Abundance")
shannon <- readRDS("Data/gam_shannon_results_parametric_model_df.RDS") %>%
  mutate(analysis="Shannon diversity")
phylo <- readRDS("Data/gam_phylo_results_parametric_model_df.RDS") %>%
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

# see the sample size for each analysis
analysis_df %>%
  replace_na(list(term = 0)) %>%
  dplyr::filter(term != 0) %>%
  group_by(analysis) %>%
  summarize(N=n()/5)



### Trying stuff
analysis_df %>%
  group_by(city, analysis) %>%
  dplyr::filter(term != "(Intercept)") %>%
  arrange(desc(Estimate)) %>%
  mutate(largest=case_when(
    abs(Estimate[1]) > abs(Estimate[4]) ~ abs(Estimate[1]),
    abs(Estimate[1]) < abs(Estimate[4]) ~ abs(Estimate[4])
  )) %>%
  mutate(proportional_estimate=Estimate/largest) %>%
  mutate(term=case_when(
    term=="urbanness" ~ "Urbanness",
    term=="tree_mean.list" ~ "Trees",
    term=="proportion_water.list" ~ "Water",
    term=="mean_EVI.list" ~ "EVI"
  )) %>%
  ggplot(., aes(x=term, y=proportional_estimate))+
  geom_violin()+
  stat_summary(fun.y="mean", geom="point")+
  facet_wrap(~analysis)+
  theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA))+
  theme(axis.text.x=element_text(color="black"))+
  theme(axis.text.y=element_text(color="black"))+
  theme(axis.title=element_text(color="black"))+
  ylab("Proportional effect size")+
  xlab("Term")+
  coord_flip()


ggsave(filename="Figures/violin_plots_of_proportional_effect_sizes.png", 
       width=5, height=3.7, units="in")





#### Trying stuff
analysis_df %>%
  dplyr::filter(term=="proportion_water.list") %>%
  group_by(analysis, city) %>%
  mutate(case=case_when(
    significance == "Yes" & Estimate > 0 ~ "positive_significant",
    significance == "No" & Estimate > 0 ~ "positive_not_significant",
    significance == "Yes" & Estimate < 0 ~ "negative_significant",
    significance == "No" & Estimate < 0 ~ "negative_not_significant"
  )) %>%
  group_by(analysis, case) %>%
  summarize(N=n())
  














