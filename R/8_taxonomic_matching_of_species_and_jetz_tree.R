## Taxonomic sorting for clements vs. Jetz

## packages
library(readr)
library(dplyr)
library(tidyr)

## Read in clements
clements <- read_csv("eBird data/Clements-Checklist-v2018-August-2018.csv") %>%
  dplyr::filter(category == "species") %>%
  rename(COMMON_NAME = `English name`) %>%
  rename(SCIENTIFIC_NAME = `scientific name`) %>%
  dplyr::select(COMMON_NAME, SCIENTIFIC_NAME, order, family)

## Read in taxonomy from jetz
jetz <- read_csv("Phylo data/2012-03-04206D-master_taxonomy.csv") %>%
  rename(SCIENTIFIC_NAME = Scientific) %>%
  rename(COMMON_NAME = English)

## Get a list of all distinct species
## among all cities
## some species will likely be dropped from analysis
## but this is the largest possible set that could be included


## first get list of all species in analysis
list_of_species_for_each_city <- function(x) {
  
  df <- readRDS(paste0("eBird data/rds for each city/", x)) %>%
    left_join(., clements, by="COMMON_NAME") %>%
    dplyr::filter(family != "Stercorariidae (Skuas and Jaegers)") %>%
    dplyr::filter(family != "Alcidae (Auks, Murres, and Puffins)") %>%
    dplyr::filter(family != "Diomedeidae (Albatrosses)") %>%
    dplyr::filter(family != "Oceanitidae (Southern Storm-Petrels)") %>%
    dplyr::filter(family != "Hydrobatidae (Northern Storm-Petrels)") %>%
    dplyr::filter(family != "Procellariidae (Shearwaters and Petrels)") %>%
    dplyr::filter(family != "Fregatidae (Frigatebirds)") %>%
    dplyr::filter(family != "Sulidae (Boobies and Gannets)")
  
  city_name <- gsub(".RDS", "", x)
  
  list_of_species <- df %>%
    group_by(COMMON_NAME) %>%
    summarise(N=n()) %>%
    mutate(city=city_name)
  
}


## apply function
files <- list.files("eBird data/rds for each city/")

list <- lapply(files, function(x) list_of_species_for_each_city(x))

species <- do.call(rbind.data.frame, list) %>%
  distinct()

study_species <- species %>% 
  dplyr::select(COMMON_NAME) %>% 
  distinct()

## get scientific family for each of the potential species
study_species <- study_species %>%
  left_join(., clements, by="COMMON_NAME")


write_csv(study_species, "eBird data/list_of_all_potential_species.csv")




## merge with jetz tree by scientific name and
## then get list of species which are not resolved
## first do a merge by scientific name
## then merge by common name
## then merge by BOTH common and scientific name
## then see which species are not on any of these procedures
scientific_merge <- study_species %>%
  left_join(., jetz, by="SCIENTIFIC_NAME") %>%
  rename(COMMON_NAME.ebird = COMMON_NAME.x) %>%
  rename(COMMON_NAME.jetz = COMMON_NAME.y) %>%
  replace_na(list(TipLabel="X")) %>%
  dplyr::filter(TipLabel != "X")

common_merge <- study_species %>%
  left_join(., jetz, by="COMMON_NAME") %>%
  rename(SCIENTIFIC_NAME.ebird = SCIENTIFIC_NAME.x) %>%
  rename(SCIENTIFIC_NAME.jetz = SCIENTIFIC_NAME.y) %>%
  replace_na(list(TipLabel="X")) %>%
  dplyr::filter(TipLabel != "X")

combined1 <- common_merge %>%
  rename(COMMON_NAME.ebird = COMMON_NAME) %>%
  dplyr::select(-SCIENTIFIC_NAME.ebird, -SCIENTIFIC_NAME.jetz) %>%
  full_join(., scientific_merge, by="COMMON_NAME.ebird") %>%
  distinct(COMMON_NAME.ebird, .keep_all = TRUE) %>%
  replace_na(list(TipLabel.y = "NO_JOIN", TipLabel.x = "NO_JOIN"))

# get list of species which only can be joined by
# scientific name with jetz
scientific_only_list <- combined1 %>%
  dplyr::filter(TipLabel.x=="NO_JOIN") %>%
  dplyr::select(COMMON_NAME.ebird) %>%
  .$COMMON_NAME.ebird

# get list of species which only can be joined
# by common name with jetz
common_only_list <- combined1 %>%
  dplyr::filter(TipLabel.y=="NO_JOIN") %>%
  dplyr::select(COMMON_NAME.ebird) %>%
  .$COMMON_NAME.ebird


# now create a dataframe of each
# of three scenarios
# common only
# scientific only
# either
common_only <- study_species %>%
  dplyr::filter(COMMON_NAME %in% common_only_list) %>%
  left_join(., jetz, by="COMMON_NAME") %>%
  rename(SCIENTIFIC_NAME = SCIENTIFIC_NAME.x) %>%
  dplyr::select(-SCIENTIFIC_NAME.y)


scientific_only <- study_species %>%
  dplyr::filter(COMMON_NAME %in% scientific_only_list) %>%
  left_join(., jetz, by="SCIENTIFIC_NAME") %>%
  rename(COMMON_NAME = COMMON_NAME.x) %>%
  dplyr::select(-COMMON_NAME.y)

either <- study_species %>%
  inner_join(., jetz, by=c("COMMON_NAME", "SCIENTIFIC_NAME"))

# this is a list of all potential species which matched in one
# of the above scenarios
combined_matched <- bind_rows(common_only, scientific_only, either)

# a list of species which did not match at all
not_matched <- combined_matched %>%
  dplyr::select(COMMON_NAME, SCIENTIFIC_NAME) %>%
  mutate(matched="yes") %>%
  right_join(., study_species, by=c("COMMON_NAME", "SCIENTIFIC_NAME")) %>%
  replace_na(list(matched="no")) %>%
  dplyr::filter(matched == "no") %>%
  dplyr::select(COMMON_NAME)

## now need to manually add these species and get the right name on Jetz
## This requires a bit of research. Some of these
matching_fix <- data.frame(COMMON_NAME.ebird=not_matched$COMMON_NAME,
                           COMMON_NAME.jetz=c("Common Moorhen",
                                              "Common Snipe",
                                              "Great Grey Shrike",
                                              "Black-throated Grey Warbler",
                                              "Winter Wren",
                                              "Kentish Plover",
                                              "Whip-poor-will",
                                              "Western Scrub-jay",
                                              "Mallard",
                                              "Whip-poor-will",
                                              "Sage Sparrow",
                                              "Le Conte's Sparrow",
                                              "Sage Sparrow",
                                              "Purple Swamphen",
                                              "Grey Hawk",
                                              "Clapper Rail",
                                              "Grey-necked Wood-rail",
                                              "Yellow Wagtail",
                                              "White-collared Seedeater",
                                              "Bronze Munia",
                                              "White-throated Munia",
                                              "Magpie Munia",
                                              "Red-billed Hornbill",
                                              "Spotted Catbird",
                                              "Black-and-white Munia",
                                              "Red Crossbill"))



# now intersect the remaining missing species with the jetz tree
matching_remaining <- matching_fix %>%
  rename(COMMON_NAME = COMMON_NAME.jetz) %>%
  left_join(., jetz, by="COMMON_NAME") %>%
  dplyr::select(-COMMON_NAME, -SCIENTIFIC_NAME) %>%
  rename(COMMON_NAME = COMMON_NAME.ebird) %>%
  left_join(., study_species, by="COMMON_NAME") %>%
  dplyr::select(1, 10:12, 2:9)

## combine with the already combined file above for a final taxonomic
## key for the phylogenetic diversity calculations
final_taxonomic_file_for_PD <- bind_rows(combined_matched, matching_remaining)

## write it out as a csv to the Phylo Data folder
write_csv(final_taxonomic_file_for_PD, "Phylo data/eBird_jetz_taxo_key.csv")




