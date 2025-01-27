## ----load_packages------------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(ggplot2) # create data visualizations
library(stringr) # work with string
library(lubridate) # manipulate dates
library(here) # file paths
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps
library(basemaps) # access open source basemaps
library(tigris) # access TIGER/Line shapefiles


## ----load_inat_csv------------------------------------------------------------

inat <- read_csv(here("data/cleaned/cnc-los-angeles-observations.csv"))


## -----------------------------------------------------------------------------
inat %>%
  filter(user_login == 'natureinla' & 
           quality_grade == 'research') %>%
  select(user_login, common_name, scientific_name, observed_on)


## ----top_ten_species----------------------------------------------------------

top_10 <- inat %>%
  select(common_name, scientific_name) %>%
  count(common_name, scientific_name, name='count')  %>%
  arrange(desc(count)) %>%
  slice(1:10)

top_10


## ----map_of_western_fence_lizard----------------------------------------------

lizard <- inat %>%
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326, remove=FALSE) %>%
  select(id, user_login, common_name, scientific_name, observed_on,  url, longitude, latitude, geometry) %>%
  filter(common_name == 'Western Fence Lizard')


mapview(lizard)


## ----chart_observations_per_year----------------------------------------------

inat %>%
  mutate(year = year(observed_on))  %>%
  group_by(year) %>%
  ggplot(aes(x = year)) +
  geom_bar()


