## ----load_packages--------------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data

library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps
library(here) # file paths

source(here('scripts/data_utils.R'))


## ----fix_sf_bug_1---------------------------------------------------------------
sf_use_s2(FALSE)


## ----read_data_from_csv---------------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))



## ----get_column_names-----------------------------------------------------------
names(inat_data)


## ----select_columns-------------------------------------------------------------
inat_base_sf <- inat_data |>
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326)


## ----show_columns---------------------------------------------------------------
names(inat_base_sf)


## ----show_classes---------------------------------------------------------------
class(inat_base_sf)


## ----examine_inat_base_sf_crs---------------------------------------------------
st_crs(inat_base_sf)


## ----select_inat_base_sf_columns------------------------------------------------
 inat_sf <- inat_base_sf |>
  select(user_login, common_name, scientific_name, observed_on,  url, quality_grade)


## ----size_of_dataframe----------------------------------------------------------
dim(inat_sf)


## ----get_oak_data---------------------------------------------------------------
inat_oak_sf <- inat_sf |>
  filter(scientific_name == 'Quercus agrifolia')


## ----get_size_of_oak_dataframe--------------------------------------------------
dim(inat_oak_sf)


## ----create_static_map_for_oak--------------------------------------------------
ggplot() +
  geom_sf(data = inat_oak_sf)


## ----create_interactive_map-----------------------------------------------------
mapview(inat_oak_sf)


## ----exercise_create_map_one_species--------------------------------------------


## ----get_LA_County_boundaries---------------------------------------------------
la_county_sf <- read_sf(here('data/raw/LA_County_Boundary/LA_County_Boundary.shp'))


## ----class_la_county_sf.--------------------------------------------------------
class(la_county_sf)


## ----glimspe_la_county_sf-------------------------------------------------------
glimpse(la_county_sf)


## ----map_la_county--------------------------------------------------------------
ggplot() +
  geom_sf(data = la_county_sf)


## ----check_la_county_and_inat_crs-----------------------------------------------
st_crs(la_county_sf) == st_crs(inat_oak_sf)


## ----update_la_county_crs-------------------------------------------------------
la_county_sf <- st_transform(la_county_sf,  crs = st_crs(inat_oak_sf))

st_crs(la_county_sf) == st_crs(inat_oak_sf)


## ----add_LA_County_to_static_map------------------------------------------------
ggplot() +
  geom_sf(data = la_county_sf)  +
  geom_sf(data = inat_oak_sf)


## ----check_la_county_geometry---------------------------------------------------
la_county_sf$geometry[1]


## ----check_inat_geometry--------------------------------------------------------
inat_oak_sf$geometry[1]


## ----create_static_map_for_oak_use_color----------------------------------------
ggplot() +
  geom_sf(data = la_county_sf, color="black", fill='beige')  +
  geom_sf(data = inat_oak_sf, color='green')


## ----create_static_map_for_oak_use_alpha----------------------------------------
ggplot() +
  geom_sf(data = la_county_sf, color="black", fill=alpha('beige', .5))  +
  geom_sf(data = inat_oak_sf, color=alpha('green', .3))


## ----create_static_map_for_oak_use_quality_grade--------------------------------

ggplot() +
  geom_sf(data = la_county_sf, color="black", fill='beige')  +
  geom_sf(data = inat_oak_sf, mapping=aes(color=quality_grade))



## ----add_title_to_static_map----------------------------------------------------
ggplot() +
  geom_sf(data = la_county_sf, color="black", fill='beige')  +
  geom_sf(data = inat_oak_sf, mapping=aes(color=quality_grade)) +
  labs(title = 'CNC observations for Live Coast Oaks in LA County',
       subtitle='2016-2024',
       color='Quality Grade') +
  theme_void()



## ----add_LA_County_to_interactive_map-------------------------------------------
mapview(la_county_sf) +
  mapview(inat_oak_sf)


## ----add_LA_County_to_interactive_map_remove_legend-----------------------------
mapview(la_county_sf, legend=FALSE) +
  mapview(inat_oak_sf, legend=FALSE)


## ----add_LA_County_to_interactive_map_remove_popup------------------------------
mapview(la_county_sf, legend=FALSE, popup=FALSE, label=FALSE) +
  mapview(inat_oak_sf, legend=FALSE)


## ----create_interactive_map_use_color-------------------------------------------
mapview(la_county_sf,
        legend=FALSE, popup=FALSE, label=FALSE,
        color='black', col.regions='beige') +
  mapview(inat_oak_sf,
          legend=FALSE,
          color='black', col.regions='green')


## ----turn_off_color_shuffle-----------------------------------------------------
mapviewOptions(basemaps.color.shuffle = FALSE)


## ----create_interactive_map_set_opacity-----------------------------------------
mapview(la_county_sf, legend=FALSE,
        popup=FALSE, label=FALSE,
        color='black', col.regions='beige',
         alpha.region=1) +
  mapview(inat_oak_sf, legend=FALSE,
          color='black', col.regions='green',
          alpha.region=1)


## ----create_interactive_map_and_show_quality_grade------------------------------

mapview(la_county_sf, legend=FALSE,
        popup=FALSE, label=FALSE,
        color='black', col.regions='beige') +
  mapview(inat_oak_sf, zcol='quality_grade')


## ----create_interactive_map_and_rename_legend_title-----------------------------
mapview(la_county_sf, legend=FALSE,
        popup=FALSE, label=FALSE,
        color='black', col.regions='beige') +
  mapview(inat_oak_sf, zcol='quality_grade',
          layer.name='Quality Grade')



## ----exercise_create_map_one_species_county_boundary----------------------------


## ----get_Expo_park_boundaries---------------------------------------------------
expo_park_boundary <- read_sf(here('data/raw/boundaries_expo_park_area.geojson'))


## ----glimpse_expo_park_boundary-------------------------------------------------
glimpse(expo_park_boundary)


## ----check_crs------------------------------------------------------------------
st_crs(expo_park_boundary) == st_crs(inat_sf)


## ----create_static_map_expo_park------------------------------------------------
ggplot() +
  geom_sf(data = expo_park_boundary)


## ----create_interactive_map_expo_park-------------------------------------------
mapview(expo_park_boundary)


## ----get_all_observations_within_expo_park--------------------------------------
inat_expo <- st_filter(inat_sf, expo_park_boundary)

dim(inat_expo)


## ----create_static_map_of_observations_in_expo_park-----------------------------
ggplot() +
  geom_sf(data = expo_park_boundary)  +
  geom_sf(data = inat_expo)


## ----create_interactive_map_of_observations_in_expo_park------------------------
mapview(expo_park_boundary) +
  mapview(inat_expo)



## ----top_ten_species_expo_park--------------------------------------------------

inat_expo |>
  st_drop_geometry() |>
  count(common_name, scientific_name) |>
  arrange(desc(n)) |>
  slice(1:10)


## ----exercise_create_map_of_observations_inside_boundary------------------------


## ----read_la_river_file---------------------------------------------------------
la_river <- read_sf(here('data/cleaned/los_angeles_river.geojson'))


## ----check_la_river_crs---------------------------------------------------------
st_crs(la_river) == st_crs(inat_sf)


## ----change_la_river_crs--------------------------------------------------------
la_river <- st_transform(la_river, crs = st_crs(inat_sf))

st_crs(la_river) == st_crs(inat_sf)


## ----change_crs_to_5070---------------------------------------------------------
river_5070 <- st_transform(la_river, crs=5070)


## ----create_buffer--------------------------------------------------------------
buffer_river_5070 <- st_buffer(river_5070, 805)


## ----change_crs_to_4326---------------------------------------------------------
buffer_river <- st_transform(buffer_river_5070, crs=st_crs(inat_sf))


## ----create_map_with_la_river_and_buffer----------------------------------------

mapview(buffer_river) +
  mapview(la_river)


## ----get_all_observations_near_la_river-----------------------------------------

inat_data_2 <- inat_base_sf |>
  select(user_login, common_name, scientific_name, taxon_kingdom_name)

inat_river <- st_filter(inat_data_2, buffer_river)



## ----create_map_with_la_river_nearby_observations-------------------------------

mapview(buffer_river, legend=FALSE,
        popup=FALSE, label=FALSE) +
  mapview(la_river, legend = FALSE) +
  mapview(inat_river, zcol='taxon_kingdom_name')


## ----top_ten_species_la_river---------------------------------------------------

inat_river |>
  st_drop_geometry() |>
  count(common_name, scientific_name) |>
  arrange(desc(n)) |>
  slice(1:10)


## ----facet_kingdom_map----------------------------------------------------------
ggplot() +
  geom_sf(data = inat_river ) +
  facet_wrap(vars(taxon_kingdom_name))


## ----read_neighborhood_file-----------------------------------------------------
la_neighborhoods_sf <- read_sf(here('data/raw/la_times_la_county_neighborhoods.json'))

glimpse(la_neighborhoods_sf)



## ----create_map_of_la_neighborhoods---------------------------------------------
ggplot(la_neighborhoods_sf) +
  geom_sf()



## ----select_columns_in_neighborhood---------------------------------------------
la_neighborhoods_sf <- la_neighborhoods_sf |>
  select(name, geometry)

head(la_neighborhoods_sf)


## ----check_neighborhood_inat_crs------------------------------------------------
st_crs(la_neighborhoods_sf) == st_crs(inat_sf)


## ----add_inat_count_to_neighborhood_sf------------------------------------------
count_sf <- add_inat_count_to_boundary_sf(inat_sf, la_neighborhoods_sf, 'name')

glimpse(count_sf)


## ----view_count_sf--------------------------------------------------------------
head(count_sf)


## ----create_static_choropleth_map-----------------------------------------------
ggplot() +
  geom_sf(data = count_sf, mapping =aes(fill = observations_count))


## ----create_interactive_choropleth_map------------------------------------------
mapview(count_sf,
        zcol = 'observations_count')


## ----save_static_map------------------------------------------------------------

# create map
my_map <- ggplot() +
  geom_sf(data = expo_park_boundary)  +
  geom_sf(data = inat_expo)

# save map
ggsave(filename = here('results/expo_park_observations.jpg'), plot = my_map,  height = 6, width = 8)

