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
  filter(taxon_kingdom_name == 'Animalia',
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
write_csv(heron_obs, here('results/heron_observations.csv'), na='')


## -----------------------------------------------------------------------------
heron_obs_sf <- heron_obs %>%
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326)


## -----------------------------------------------------------------------------
mapview(heron_obs_sf)


## -----------------------------------------------------------------------------
heron_obs_sf <- heron_obs_sf %>%
  select(user_login, observed_on, common_name, taxon_species_name)



## -----------------------------------------------------------------------------
mapview(heron_obs_sf)


## -----------------------------------------------------------------------------
la_river <- read_sf(here('data/cleaned/los_angeles_river.geojson'))


## -----------------------------------------------------------------------------
st_crs(la_river) == st_crs(heron_obs_sf)


## -----------------------------------------------------------------------------
la_river <- st_transform(la_river,  crs = st_crs(heron_obs_sf))

st_crs(la_river) == st_crs(heron_obs_sf)


## -----------------------------------------------------------------------------
mapview(heron_obs_sf, col.regions='green') +
  mapview(la_river)


## -----------------------------------------------------------------------------
la_river_5070 <- st_transform(la_river, crs=5070)


## -----------------------------------------------------------------------------
buffer_la_river_5070 <- st_buffer(la_river_5070, 805)


## -----------------------------------------------------------------------------
buffer_la_river <- st_transform(buffer_la_river_5070, crs=st_crs(heron_obs_sf))


## -----------------------------------------------------------------------------
mapview(heron_obs_sf, col.regions='green') +
  mapview(la_river) +
  mapview(buffer_la_river)


## -----------------------------------------------------------------------------
heron_near_river_sf <- heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_la_river)) > 0, ]

dim(heron_near_river_sf)


## -----------------------------------------------------------------------------
final_map <- mapview(heron_near_river_sf, col.regions='green') +
  mapview(la_river) +
  mapview(buffer_la_river)

final_map


## -----------------------------------------------------------------------------
write_csv(heron_near_river_sf, here('results/heron_near_la_river.csv'), na='')


## -----------------------------------------------------------------------------
mapshot2(final_map, file = here('results/heron_near_la_river.png'))


## -----------------------------------------------------------------------------
water_areas <- read_sf(here('data/cleaned/la_county_waterareas.geojson'))


## -----------------------------------------------------------------------------
st_crs(water_areas) == st_crs(heron_obs_sf)


## -----------------------------------------------------------------------------
water_areas <- st_transform(water_areas,  crs = st_crs(heron_obs_sf))

st_crs(water_areas) == st_crs(heron_obs_sf)


## -----------------------------------------------------------------------------

mapview(heron_obs_sf, col.regions='green') +
  mapview(water_areas)


## -----------------------------------------------------------------------------
water_areas_5070 <- st_transform(water_areas, crs=5070)

buffer_water_areas_5070 <- st_buffer(water_areas_5070, 805)

buffer_water_areas <- st_transform(buffer_water_areas_5070, crs=st_crs(heron_obs_sf))



## -----------------------------------------------------------------------------
heron_near_water_sf <- heron_obs_sf %>%
  mutate(near_water=lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0)


## -----------------------------------------------------------------------------
write_csv(heron_near_water_sf, here('results/heron_near_la_river.csv'), na='')


## -----------------------------------------------------------------------------
ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water))


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
ggsave(filename = here('results/heron_observations_near_water_chart.jpg'),
       plot = final_chart, height = 6, width = 8)


## -----------------------------------------------------------------------------
LA_county <- read_sf(here('data/cleaned/los_angeles_county/los_angeles_county.shp'))


## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399')



## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
ggsave(filename = here('results/heron_observations_near_water_map.jpg'),
       plot = final_map, height = 6, width = 8)


## -----------------------------------------------------------------------------

## load_packages

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(here) # file paths
library(stringr) # work with string

library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps

source(here('scripts/data_utils.R'))

## Select City Nature Challenge observations

inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))

names(inat_data)

heron_obs <- inat_data %>%
  filter(taxon_kingdom_name == 'Animalia',
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

write_csv(heron_obs, here('results/heron_observations.csv'), na='')

## Create a map with CNC observations

heron_obs_sf <- heron_obs %>%
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326)

mapview(heron_obs_sf)

heron_obs_sf <- heron_obs_sf %>%
  select(user_login, observed_on, common_name, taxon_species_name)

mapview(heron_obs_sf)

## Add LA River to the map

la_river <- read_sf(here('data/cleaned/los_angeles_river.geojson'))

st_crs(la_river) == st_crs(heron_obs_sf)

la_river <- st_transform(la_river,  crs = st_crs(heron_obs_sf))

st_crs(la_river) == st_crs(heron_obs_sf)

mapview(heron_obs_sf, col.regions='green') +
  mapview(la_river)


## Observations near LA River

la_river_5070 <- st_transform(la_river, crs=5070)

buffer_la_river_5070 <- st_buffer(la_river_5070, 805)

buffer_la_river <- st_transform(buffer_la_river_5070, crs=st_crs(heron_obs_sf))

mapview(heron_obs_sf, col.regions='green') +
  mapview(la_river) +
  mapview(buffer_la_river)

heron_near_river_sf <- heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_la_river)) > 0, ]

dim(heron_near_river_sf)

final_map <- mapview(heron_near_river_sf, col.regions='green') +
  mapview(la_river) +
  mapview(buffer_la_river)

final_map

write_csv(heron_near_river_sf, here('results/heron_near_la_river.csv'), na='')

mapshot2(final_map, file = here('results/heron_near_la_river.png'))

## Observations near bodies of water

water_areas <- read_sf(here('data/cleaned/la_county_waterareas.geojson'))

st_crs(water_areas) == st_crs(heron_obs_sf)

water_areas <- st_transform(water_areas,  crs = st_crs(heron_obs_sf))

st_crs(water_areas) == st_crs(heron_obs_sf)

mapview(heron_obs_sf, col.regions='green') +
  mapview(water_areas)

water_areas_5070 <- st_transform(water_areas, crs=5070)

buffer_water_areas_5070 <- st_buffer(water_areas_5070, 805)

buffer_water_areas <- st_transform(buffer_water_areas_5070, crs=st_crs(heron_obs_sf))

heron_obs_sf <- heron_obs_sf %>%
  mutate(near_water=lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0)

ggplot()+
  geom_bar(data=heron_obs_sf, mapping=aes(x=near_water))

final_chart <- ggplot()+
  geom_bar(data=heron_obs_sf, mapping=aes(x=near_water)) +
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

LA_county <- read_sf(here('data/cleaned/los_angeles_county/los_angeles_county.shp'))

ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_obs_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399')


final_map <- ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_obs_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399') +
    labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       color='Within 1/2 mile of water') +
  theme_void() +
  theme(title = element_text(size = 13))

final_map

ggsave(filename = here('results/heron_observations_near_water_map.jpg'),
       plot = final_map, height = 6, width = 8)


