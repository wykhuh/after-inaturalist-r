## ----loading_packages---------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(here) # file paths


## ----assign_read_csv_to_object------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))


## ----filter_oaks_observations-------------------------------------------------
oaks_obs <- inat_data %>%
  filter(common_name == 'oaks')

dim(oaks_obs)


## ----use_names_to_get_fields_2------------------------------------------------
names(inat_data)


## ----get_quercus_observations-------------------------------------------------
oaks_obs_fixed <- inat_data %>%
  filter(
    taxon_kingdom_name == 'Plantae' &
    taxon_phylum_name == 'Tracheophyta' &
    taxon_class_name == 'Magnoliopsida' &
    taxon_order_name == 'Fagales' &
    taxon_family_name == 'Fagaceae' &
    taxon_genus_name == 'Quercus'
  )

dim(oaks_obs_fixed)


## ----unique_oak_common_names--------------------------------------------------
unique(oaks_obs_fixed$common_name)


## ----get_Eisenia_observations-------------------------------------------------
Eisenia_obs <- inat_data %>%
  filter(taxon_genus_name == 'Eisenia') %>%
  select(common_name, taxon_kingdom_name)

Eisenia_obs


## ----get_Plantae_Quercus_observations-----------------------------------------
Plantae_Quercus_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Plantae' &
           taxon_genus_name == 'Quercus') %>%
  select(common_name, taxon_kingdom_name)

dim(Plantae_Quercus_obs)


## ----get_Tracheophyta_observations--------------------------------------------
trees_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Plantae' &
           taxon_phylum_name == 'Tracheophyta')

dim(trees_obs)


## ----unique_trees_common_name-------------------------------------------------
unique(trees_obs$common_name)[0:30]


## ----get_laco_species_observations--------------------------------------------
laco_species <- c('Acacia aneura', 'Acacia stenophylla', 'Afrocarpus falcatus', "Agonis flexuosa", 'Angophora costata', "Arbutus 'marina'", 'Arbutus unedo'  )

laco_species_obs <- inat_data %>%
  filter(taxon_species_name %in% laco_species &
           taxon_kingdom_name == 'Plantae') %>%
  select(user_login, common_name, scientific_name, taxon_species_name)


## ----get_laco_genera_observations---------------------------------------------
laco_genera <- c('Acacia',  'Afrocarpus', "Agonis", 'Angophora', "Arbutus" )

laco_genera_obs <- inat_data %>%
  filter(taxon_genus_name %in% laco_genera &
           taxon_kingdom_name == 'Plantae') %>%
  select(user_login, common_name, scientific_name, taxon_genus_name)

