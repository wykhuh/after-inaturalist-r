## -----------------------------------------------------------------------------


## ----load_packages------------------------------------------------------------
library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(here) # file paths
library(stringr) # work with string

library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps

source(here('scripts/data_utils.R'))


## ----read_inat_file-----------------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))



## -----------------------------------------------------------------------------
names(inat_data)


## -----------------------------------------------------------------------------
heron_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Animalia' &
         taxon_species_name == 'Ardea herodias')

dim(heron_obs)


## -----------------------------------------------------------------------------
table(heron_obs$quality_grade)


## -----------------------------------------------------------------------------
table(heron_obs$coordinates_obscured)


## -----------------------------------------------------------------------------
heron_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Animalia',
         taxon_species_name == 'Ardea herodias') %>%
  filter(quality_grade == 'research') %>%
  filter(coordinates_obscured == FALSE)


dim(heron_obs)


## -----------------------------------------------------------------------------
unique(heron_obs$taxon_kingdom_name)
unique(heron_obs$taxon_species_name)
unique(heron_obs$quality_grade)
unique(heron_obs$coordinates_obscured)


## -----------------------------------------------------------------------------
write_csv(heron_obs, here('results/heron_observations.csv'), na='')


## -----------------------------------------------------------------------------
heron_obs_sf <- heron_obs %>%
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326)


## -----------------------------------------------------------------------------
mapview(heron_obs_sf)


## -----------------------------------------------------------------------------
heron_map <- heron_obs_sf %>%
  select(user_login, observed_on, common_name, taxon_species_name, image_url)



## -----------------------------------------------------------------------------
mapview(heron_map)


## -----------------------------------------------------------------------------
water_areas <- read_sf(here('data/cleaned/la_county_waterareas.geojson'))


## -----------------------------------------------------------------------------
st_crs(water_areas) == st_crs(heron_obs_sf)


## -----------------------------------------------------------------------------
water_areas <- st_transform(water_areas,  crs = st_crs(heron_obs_sf))

st_crs(water_areas) == st_crs(heron_obs_sf)


## -----------------------------------------------------------------------------
mapview(heron_map, col.regions='green') +
  mapview(water_areas)


## -----------------------------------------------------------------------------
water_areas_5070 <- st_transform(water_areas, crs=5070)


## -----------------------------------------------------------------------------
buffer_water_areas_5070 <- st_buffer(water_areas_5070, 805)


## -----------------------------------------------------------------------------
buffer_water_areas <- st_transform(buffer_water_areas_5070, crs=st_crs(heron_obs_sf))


## -----------------------------------------------------------------------------
mapview(heron_obs_sf, col.regions='green') +
  mapview(buffer_water_areas)


## -----------------------------------------------------------------------------
lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0


## -----------------------------------------------------------------------------
heron_near_water_sf <- heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0, ]

dim(heron_near_water_sf)


## -----------------------------------------------------------------------------
mapview(heron_near_water_sf, col.regions='green') +
  mapview(buffer_water_areas)



## -----------------------------------------------------------------------------
heron_map <- mapview(heron_near_water_sf, col.regions='green') +
  mapview(buffer_water_areas)


## -----------------------------------------------------------------------------

mapshot2(heron_map, file = here('results/heron_near_water_draft.png'))


## -----------------------------------------------------------------------------
heron_near_water_sf <- heron_obs_sf %>%
  mutate(near_water = lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0)


## -----------------------------------------------------------------------------
table(heron_near_water_sf$near_water)


## -----------------------------------------------------------------------------
write_csv(heron_near_water_sf, here('results/heron_near_water.csv'), na='')


## -----------------------------------------------------------------------------
ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water))


## -----------------------------------------------------------------------------
heron_chart <- ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water)) +
  labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       x='Within 1/2 mile of water',
       y='observations count')  +
  theme_bw() +
  theme(title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

heron_chart


## -----------------------------------------------------------------------------
ggsave(filename = here('results/heron_observations_near_water_chart.jpg'),
       plot = heron_chart, height = 6, width = 8)


## -----------------------------------------------------------------------------
LA_county <- read_sf(here('data/cleaned/los_angeles_county/los_angeles_county.shp'))



## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399')



## -----------------------------------------------------------------------------
heron_map <- ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399') +
    labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       color='Within 1/2 mile of water') +
  theme_void() +
  theme(title = element_text(size = 13))

heron_map


## -----------------------------------------------------------------------------
ggsave(filename = here('results/heron_observations_near_water_map.jpg'),
       plot = heron_map, height = 6, width = 8)


## ----download_images----------------------------------------------------------

heron_images <- heron_near_water_sf %>%
  filter(license %in% c('CC0', 'CC-BY', 'CC-BY-NC')) %>%
  slice_sample(n=3)

download_inaturalist_images(heron_images)


## -----------------------------------------------------------------------------

## =================
## load_packages
## =================

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(here) # file paths
library(stringr) # work with string

library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps

source(here('scripts/data_utils.R'))

## =================
## Select City Nature Challenge observations
## =================

inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))

names(inat_data)

heron_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Animalia' &
         taxon_species_name == 'Ardea herodias')

dim(heron_obs)

table(heron_obs$quality_grade)

table(heron_obs$coordinates_obscured)

heron_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Animalia',
         taxon_species_name == 'Ardea herodias') %>%
  filter(quality_grade == 'research') %>%
  filter(coordinates_obscured == FALSE)

dim(heron_obs)

unique(heron_obs$taxon_kingdom_name)
unique(heron_obs$taxon_species_name)
unique(heron_obs$quality_grade)
unique(heron_obs$coordinates_obscured)

write_csv(heron_obs, here('results/heron_observations.csv'), na='')


## =================
## Create a map with CNC observations
## =================

heron_obs_sf <- heron_obs %>%
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326)

mapview(heron_obs_sf)

heron_map <- heron_obs_sf %>%
  select(user_login, observed_on, common_name, taxon_species_name, image_url)

mapview(heron_map)


## =================
## Add bodies of water to the map
## =================

water_areas <- read_sf(here('data/cleaned/la_county_waterareas.geojson'))

st_crs(water_areas) == st_crs(heron_obs_sf)

water_areas <- st_transform(water_areas,  crs = st_crs(heron_obs_sf))

st_crs(water_areas) == st_crs(heron_obs_sf)

mapview(heron_map, col.regions='green') +
  mapview(water_areas)

## =================
## Observations near bodies of water
## =================

water_areas_5070 <- st_transform(water_areas, crs=5070)

buffer_water_areas_5070 <- st_buffer(water_areas_5070, 805)

buffer_water_areas <- st_transform(buffer_water_areas_5070, crs=st_crs(heron_obs_sf))

heron_map <- mapview(heron_map, col.regions='green') +
  mapview(water_areas) +
  mapview(buffer_water_areas)

heron_map

heron_near_water_sf <- heron_obs_sf %>%
  mutate(near_water=lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0)

table(heron_near_water_sf$near_water)

write_csv(heron_near_water_sf, here('results/heron_near_water.csv'), na='')

mapshot2(heron_map, file = here('results/heron_near_water_draft.png'))


## =================
## Create chart
## =================

ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water))

final_chart <- ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water)) +
  labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       x='Within 1/2 mile of water',
       y='observations count')  +
  theme_bw() +
  theme(title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

final_chart

ggsave(filename = here('results/heron_observations_near_water_chart.jpg'),
       plot = final_chart, height = 6, width = 8)

## =================
## Create map
## =================

LA_county <- read_sf(here('data/cleaned/los_angeles_county/los_angeles_county.shp'))

ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399')

final_map <- ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399') +
    labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       color='Within 1/2 mile of water') +
  theme_void() +
  theme(title = element_text(size = 13))

final_map

ggsave(filename = here('results/heron_observations_near_water_map.jpg'),
       plot = final_map, height = 6, width = 8)


## =================
## Download iNaturalist images
## =================

heron_images <- heron_near_water_sf %>%
  filter(license %in% c('CC0', 'CC-BY', 'CC-BY-NC')) %>%
  slice_sample(n=3)

download_inaturalist_images(heron_images)


