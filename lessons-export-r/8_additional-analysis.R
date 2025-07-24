## ----load_packages------------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps
library(basemaps) # add basemap
library(here) # file paths

source(here('scripts/data_utils.R'))


## ----fix_sf_bug---------------------------------------------------------------
sf_use_s2(FALSE)


## ----read_data_from_csv-------------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))



## ----add_year_column----------------------------------------------------------
inat_year <- inat_data %>%
  mutate(year = year(observed_on))


## ----select_columns-----------------------------------------------------------
inat_sf <- inat_data %>%
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326) %>%
  select(user_login, common_name, scientific_name, observed_on,  url, quality_grade)


## ----create_dodged_bar_chart--------------------------------------------------
ggplot(data = inat_year ,
       mapping = aes(x = year, fill = quality_grade))  +
  geom_bar(position = position_dodge(preserve = 'single'))


## ----create_dataframe_for_multi_line_chart------------------------------------
year_quality_count <- inat_data %>%
  mutate(year = year(observed_on))  %>%
  count(year, quality_grade,  name='count')

year_quality_count


## ----create_multi_line_chart--------------------------------------------------
ggplot(data = year_quality_count,
       mapping = aes(x = year, y = count, color = quality_grade)) +
  geom_line()


## ----create_dataframe_with_year_count-----------------------------------------
inat_year_count <- inat_data %>%
  mutate(year = year(observed_on)) %>%
  count(year, name='count')

inat_year_count


## ----plot_column_line_for_year_count------------------------------------------
ggplot(data = inat_year_count,
       mapping = aes(x = year, y = count)) +
  geom_col() +
  geom_line()



## ----bar_and_line_chart-------------------------------------------------------
ggplot() +
  geom_bar(data = inat_year ,
       mapping = aes(x = year, fill = quality_grade),
       position = position_dodge(preserve = 'single')) +
  geom_line(data = inat_year_count,
       mapping = aes(x = year, y = count))



## ----ge_neighborhoods---------------------------------------------------------
la_neighborhoods_sf <- read_sf(here('data/raw/la_times_la_county_neighborhoods.json'))



## ----get_expo_park------------------------------------------------------------
expo_park_sf <- la_neighborhoods_sf %>%
  filter(name=='Exposition Park')

expo_park_sf


## ----get_neighborhoods_around_expo_park---------------------------------------
expo_area_sf <- la_neighborhoods_sf[lengths(st_intersects(la_neighborhoods_sf, expo_park_sf)) > 0, ]

expo_area_sf <-  expo_area_sf %>%
  select(name)

expo_area_sf


## ----add_inat_count_to_neighborhoods------------------------------------------
expo_area_count_sf <- add_inat_count_to_boundary_sf(inat_sf, expo_area_sf, 'name')

expo_area_count_sf


## ----map_observations_per_expo_neighborhoods----------------------------------

ggplot(expo_area_count_sf, aes(label=paste0(name,': ', observations_count))) +
  geom_sf() +
  geom_sf_label(fill = "white" )



## ----update_crs---------------------------------------------------------------
expo_area_count_sf <- st_transform(expo_area_count_sf,  crs = st_crs(3857))



## ----create_map_with_basemap--------------------------------------------------

ggplot(expo_area_count_sf) +
  basemap_gglayer(expo_area_count_sf) +
  scale_fill_identity() +
  geom_sf( mapping=aes(fill=alpha("yellow", .05))) +
  geom_sf_label( mapping=aes(label = paste0(name, ': ',observations_count)) )  +
  theme_void()


## ----read_ejsm_file-----------------------------------------------------------
ejsm_sf <- read_sf(here('data/raw/EJSM_Scores-shp/6cbc6914-690f-48ec-a54f-2649a8ddb321202041-1-139ir98.m1ys.shp'))

glimpse(ejsm_sf)



## ----create_map_of_regions----------------------------------------------------
ggplot(ejsm_sf, aes(fill = CIscore)) +
  geom_sf()



## ----check_regions_inat_crs---------------------------------------------------
st_crs(ejsm_sf) == st_crs(inat_sf)


## ----update_crs_for_ejsm------------------------------------------------------
ejsm_sf <- st_transform(ejsm_sf,  crs = st_crs(inat_sf))

st_crs(ejsm_sf) == st_crs(inat_sf)


## ----create_dataframe_with_regions_inat_counts--------------------------------
ejsm_inat_sf <- add_inat_count_to_boundary_sf(inat_sf, ejsm_sf, 'OBJECTID')

glimpse(ejsm_inat_sf)


## ----create_centroids---------------------------------------------------------
centroid_sf <- st_centroid(ejsm_inat_sf) %>%
  select(OBJECTID, observations_count)

glimpse(centroid_sf)


## ----create_static_centroid_sf------------------------------------------------
ggplot() +
  geom_sf(data = centroid_sf)



## ----create_static_ejsm_centroid_map------------------------------------------
ggplot() +
  geom_sf(data=ejsm_inat_sf, aes(fill = CIscore)) +
  geom_sf(data = centroid_sf, aes(size = observations_count))



## ----create_interactive_centroid_map------------------------------------------
ejsm_inat_basic_sf <- ejsm_inat_sf %>%
  select(CIscore)

mapview(ejsm_inat_basic_sf,
        zcol = 'CIscore') +
  mapview(centroid_sf, cex="observations_count",
          zcol="observations_count",legend=FALSE, col.regions='black')


## ----examine_license----------------------------------------------------------
table(inat_data$license)


## ----select_observations_with_slice-------------------------------------------
my_inat2 <- inat_data %>%
  filter(common_name == 'Western Fence Lizard') %>%
  filter(license == 'CC0') %>%
  slice(1:5)

table(my_inat2$observed_on)


## ----select_observations_with_slice_sample------------------------------------
my_inat <- inat_data %>%
  filter(common_name == 'Western Fence Lizard') %>%
  filter(license == 'CC0') %>%
  slice_sample(n=5)

table(my_inat$observed_on)


## ----download_images----------------------------------------------------------
download_inaturalist_images(my_inat)

