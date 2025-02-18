## -----------------------------------------------------------------------------


## ----load_packages------------------------------------------------------------
library(readr) # read and write tabular data
library(dplyr) # manipulate tabular data
library(lubridate) # manipulate dates
library(here) # file paths
library(stringr) # manipulate strings

library(ggplot2) # create data visualizations like charts and maps
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps

source(here('scripts/data_utils.R')) # custom functions for workshop


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
  filter(taxon_kingdom_name == 'Animalia' &
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
  mapview(buffer_water_areas) +
  mapview(water_areas)


## -----------------------------------------------------------------------------
heron_near_water_sf <-
  heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0, ]

dim(heron_near_water_sf)


## -----------------------------------------------------------------------------
mapview(heron_near_water_sf, col.regions='green') +
  mapview(buffer_water_areas)   +
  mapview(water_areas)



## -----------------------------------------------------------------------------
heron_near_water_sf <- heron_obs_sf %>%
  mutate(near_water = lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0)


## -----------------------------------------------------------------------------
table(heron_near_water_sf$near_water)


## -----------------------------------------------------------------------------
write_csv(heron_near_water_sf, here('results/heron_near_water.csv'), na='')


## -----------------------------------------------------------------------------
ids <- c(1101584241267, 1101584241127, 1101584238995)

WNRA <- water_areas %>%
  filter(HYDROID %in% ids)


## -----------------------------------------------------------------------------
WNRA_5070 <- st_transform(WNRA, crs=5070)
buffer_WNRA_5070 <- st_buffer(WNRA_5070, 805)
buffer_WNRA <- st_transform(buffer_WNRA_5070, crs=st_crs(heron_obs_sf))



## -----------------------------------------------------------------------------
heron_WNRA_sf <- heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_WNRA)) > 0, ]

dim(heron_WNRA_sf)


## -----------------------------------------------------------------------------
mapview(heron_WNRA_sf, col.region='green') +
  mapview(WNRA)



## -----------------------------------------------------------------------------
ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water))


## -----------------------------------------------------------------------------
heron_chart <- ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water), fill='#77b100') +
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
heron_final_map <- ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399') +
    labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       color='Within 1/2 mile of water') +
  theme_void() +
  theme(title = element_text(size = 13))

heron_final_map


## -----------------------------------------------------------------------------
ggsave(filename = here('results/heron_observations_near_water_map.jpg'),
       plot = heron_final_map, height = 8, width = 8)


## -----------------------------------------------------------------------------
format_image_filename <- function(row) {
  if('scientific_name' %in% names(row)) {
    taxa = row$scientific_name
  } else if ('common_name' %in% names(row)) {
    taxa = row$common_name
  } else if ('taxon_subspecies_name' %in% names(row)) {
    taxa = row$taxon_subspecies_name
  } else if ('taxon_species_name' %in% names(row)) {
    taxa = row$taxon_species_name
  } else if ('taxon_genus_name' %in% names(row)) {
    taxa = row$taxon_genus_name
  } else if ('taxon_family_name' %in% names(row)) {
    taxa = row$taxon_family_name
  } else if ('taxon_order_name' %in% names(row)) {
    taxa = row$taxon_order_name
  } else if ('taxon_class_name' %in% names(row)) {
    taxa = row$taxon_class_name
  } else if ('taxon_phylum_name' %in% names(row)) {
    taxa = row$taxon_phylum_name
  } else if ('taxon_kingdom_name' %in% names(row)) {
    taxa = row$taxon_kingdom_name
  } else {
    taxa = ''
  }


  if('observed_on' %in% names(row)) {
    date = row$observed_on
  } else {
    date = ''
  }

  if('id' %in% names(row)) {
    id = row$id
  } else {
    id = row$row_id
  }
  if('user_login' %in% names(row)) {
    user = row$user_login
  } else {
    user = ''
  }
  if('license' %in% names(row)) {
    license = row$license
  } else {
    license = ''
  }

  # create filename
  taxa = gsub('([[:punct:]])', '', taxa)
  id = gsub('([[:punct:]])', '', id)
  user = gsub('([[:punct:]])', '', user)
  license = gsub('([[:punct:]])', '', license)

  filename = paste0(paste(taxa, date, id, user, license, sep='_'), '.jpg')
  filename = gsub(' ', '_', filename)
  filename
}

download_row_image <- function(row) {
  Sys.sleep(1)

  if(!'image_url' %in% names(row)) {
    stop('the iNaturalist data must have "image_url" column')
  }
  url = row$image_url

  # create directories
  dir.create(here('results/images'), showWarnings = FALSE)

  # download the image
  tryCatch({
    filename = format_image_filename(row)
    filepath = here('results/images', filename)
    # print(filepath)
    download.file(url, filepath)
  },
  error = function(e){
    message('Could not download image.')
    print(e)
  })
}


download_inaturalist_images <- function(inat_df) {
  temp_df <- inat_df
  temp_df$row_id <- rownames(temp_df)

  # turn observed_on to a string because apply screws up date objects
  if('observed_on' %in% names(temp_df))  {
    temp_df <- temp_df %>%
      mutate(observed_on= format(observed_on, format="%Y-%m-%d"))
  }

  apply(temp_df, 1, download_row_image)
}



## ----download_images----------------------------------------------------------

heron_images <- heron_near_water_sf %>%
  filter(license %in% c('CC0', 'CC-BY', 'CC-BY-NC')) %>%
  filter(!is.na(image_url)) %>%
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
  filter(taxon_kingdom_name == 'Animalia' &
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

## create buffer

water_areas_5070 <- st_transform(water_areas, crs=5070)

buffer_water_areas_5070 <- st_buffer(water_areas_5070, 805)

buffer_water_areas <- st_transform(buffer_water_areas_5070, crs=st_crs(heron_obs_sf))

mapview(heron_obs_sf, col.regions='green') +
  mapview(buffer_water_areas) +
  mapview(water_areas)

## observations near water, version 1

heron_near_water_sf <-
  heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0, ]

dim(heron_near_water_sf)

mapview(heron_near_water_sf, col.regions='green') +
  mapview(buffer_water_areas) +
  mapview(water_areas)


## observations near water, version 2

heron_near_water_sf <- heron_obs_sf %>%
  mutate(near_water = lengths(st_intersects(heron_obs_sf, buffer_water_areas)) > 0)

table(heron_near_water_sf$near_water)

write_csv(heron_near_water_sf, here('results/heron_near_water.csv'), na='')

### observations near lakes in Whittier Narrows Recreation Area


ids <- c(1101584241267, 1101584241127, 1101584238995)

WNRA <- water_areas %>%
  filter(HYDROID %in% ids)

WNRA_5070 <- st_transform(WNRA, crs=5070)
buffer_WNRA_5070 <- st_buffer(WNRA_5070, 805)
buffer_WNRA <- st_transform(buffer_WNRA_5070, crs=st_crs(heron_obs_sf))

heron_WNRA_sf <- heron_obs_sf[lengths(st_intersects(heron_obs_sf, buffer_WNRA)) > 0, ]

dim(heron_WNRA_sf)

mapview(heron_WNRA_sf, col.region='green') +
  mapview(WNRA)

## =================
## Create chart
## =================

ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water))

heron_chart <- ggplot()+
  geom_bar(data=heron_near_water_sf, mapping=aes(x=near_water), fill='#77b100') +
  labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       x='Within 1/2 mile of water',
       y='observations count')  +
  theme_bw() +
  theme(title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

heron_chart

ggsave(filename = here('results/heron_observations_near_water_chart.jpg'),
       plot = heron_chart, height = 8, width = 8)

## =================
## Create map
## =================

LA_county <- read_sf(here('data/cleaned/los_angeles_county/los_angeles_county.shp'))

ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399')

heron_final_map <- ggplot() +
  geom_sf(data=LA_county, fill='white') +
  geom_sf(data=heron_near_water_sf, mapping=aes(color=near_water)) +
  geom_sf(data=water_areas, fill='#007399') +
  labs(title = 'CNC observations for Great Blue Herons in LA County',
       subtitle='2016-2024',
       color='Within 1/2 mile of water') +
  theme_void() +
  theme(title = element_text(size = 13))

heron_final_map

ggsave(filename = here('results/heron_observations_near_water_map.jpg'),
       plot = heron_final_map, height = 6, width = 8)

## =================
## Download iNaturalist images
## =================

# custom functions to download iNaturalist images - begin

format_image_filename <- function(row) {
  if('scientific_name' %in% names(row)) {
    taxa = row$scientific_name
  } else if ('common_name' %in% names(row)) {
    taxa = row$common_name
  } else if ('taxon_subspecies_name' %in% names(row)) {
    taxa = row$taxon_subspecies_name
  } else if ('taxon_species_name' %in% names(row)) {
    taxa = row$taxon_species_name
  } else if ('taxon_genus_name' %in% names(row)) {
    taxa = row$taxon_genus_name
  } else if ('taxon_family_name' %in% names(row)) {
    taxa = row$taxon_family_name
  } else if ('taxon_order_name' %in% names(row)) {
    taxa = row$taxon_order_name
  } else if ('taxon_class_name' %in% names(row)) {
    taxa = row$taxon_class_name
  } else if ('taxon_phylum_name' %in% names(row)) {
    taxa = row$taxon_phylum_name
  } else if ('taxon_kingdom_name' %in% names(row)) {
    taxa = row$taxon_kingdom_name
  } else {
    taxa = ''
  }


  if('observed_on' %in% names(row)) {
    date = row$observed_on
  } else {
    date = ''
  }

  if('id' %in% names(row)) {
    id = row$id
  } else {
    id = row$row_id
  }
  if('user_login' %in% names(row)) {
    user = row$user_login
  } else {
    user = ''
  }
  if('license' %in% names(row)) {
    license = row$license
  } else {
    license = ''
  }

  # create filename
  taxa = gsub('([[:punct:]])', '', taxa)
  id = gsub('([[:punct:]])', '', id)
  user = gsub('([[:punct:]])', '', user)
  license = gsub('([[:punct:]])', '', license)

  filename = paste0(paste(taxa, date, id, user, license, sep='_'), '.jpg')
  filename = gsub(' ', '_', filename)
  filename
}

download_row_image <- function(row) {
  Sys.sleep(1)

  if(!'image_url' %in% names(row)) {
    stop('the iNaturalist data must have "image_url" column')
  }
  url = row$image_url

  # create directories
  dir.create(here('results/images'), showWarnings = FALSE)

  # download the image
  tryCatch({
    filename = format_image_filename(row)
    filepath = here('results/images', filename)
    # print(filepath)
    download.file(url, filepath)
  },
  error = function(e){
    message('Could not download image.')
    print(e)
  })
}


download_inaturalist_images <- function(inat_df) {
  temp_df <- inat_df
  temp_df$row_id <- rownames(temp_df)

  # turn observed_on to a string because apply screws up date objects
  if('observed_on' %in% names(temp_df))  {
    temp_df <- temp_df %>%
      mutate(observed_on= format(observed_on, format="%Y-%m-%d"))
  }

  apply(temp_df, 1, download_row_image)
}
# custom functions to download iNaturalist images - end


heron_images <- heron_near_water_sf %>%
  filter(license %in% c('CC0', 'CC-BY', 'CC-BY-NC')) %>%
  filter(!is.na(image_url)) %>%
  slice_sample(n=3)

download_inaturalist_images(heron_images)

