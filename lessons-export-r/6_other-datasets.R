## ----load_packages--------------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps
library(here) # file paths
library(lubridate) #


## ----fix_sf_bug_1---------------------------------------------------------------
sf_use_s2(FALSE)


## ----get_neighborhood_council---------------------------------------------------
nc_boundaries <- read_sf(here('data/raw/Neighborhood_Councils_(Certified)/Neighborhood_Councils_(Certified).shp'))


## ----view_neighborhood_council--------------------------------------------------
View(nc_boundaries)


## ----map_neighborhood_council---------------------------------------------------
ggplot() +
  geom_sf(data=nc_boundaries) +
  theme_minimal()


## ----get_arroyo_seco------------------------------------------------------------
arroyo_seco <- nc_boundaries |>
  filter(NAME == 'ARROYO SECO NC')


## ----map_arroy_seco-------------------------------------------------------------
ggplot() +
  geom_sf(data=arroyo_seco) +
  theme_minimal()


## ----save_arroyo_seco-----------------------------------------------------------
st_write(arroyo_seco, here('data/cleaned/arroyo_seco_boundaries.geojson'))


## ----get_la_times_neigbhorhoods-------------------------------------------------
la_neighborhoods <- read_sf(here('data/raw/la_times_la_county_neighborhoods.json'))


## ----map_la_times_neigbhorhoods-------------------------------------------------
ggplot() +
  geom_sf(data=la_neighborhoods)


## ----get_admin_boundaries-------------------------------------------------------
admin_boundaries <- read_sf(here('data/raw/admin_dist_SDE_DIST_DRP_CITY_COMM_BDY_-2349953032962506288/admin_dist_SDE_DIST_DRP_CITY_COMM_BDY.shp'))


## ----map_admin_boundaries-------------------------------------------------------
ggplot() +
  geom_sf(data=admin_boundaries)


## ----get_parks_data-------------------------------------------------------------
parks_national <- read_sf(here('data/cleaned/nps_la_county.geojson'))

parks_state <- read_sf(here('data/cleaned/state_parks_los_angeles_county/state_parks_los_angeles_county.shp'))

parks_county <- read_sf(here('data/raw/DPR_Park_Facilities_View_(Accessible_Parks)/DPR_Park_Facilities_View_(Accessible_Parks).shp'))

parks_city <- read_sf(here('data/raw/Los_Angeles_Recreation_and_Parks_Boundaries/Los_Angeles_Recreation_and_Parks_Boundaries.shp'))


## ----update_county_crs----------------------------------------------------------
parks_county <- st_transform(parks_county, crs = st_crs(parks_national))


## ----update_city_crs------------------------------------------------------------
parks_city <- st_transform(parks_city, crs = st_crs(parks_national))


## ----update_state_crs-----------------------------------------------------------
parks_state <- st_transform(parks_state, crs = st_crs(parks_national))



## ----map_of_parks---------------------------------------------------------------

mapview(parks_national, col.region='orange') +
  mapview(parks_county, col.region='red') +
  mapview(parks_city, col.region='yellow') +
  mapview(parks_state, col.region='green')


## ----get_water_areas------------------------------------------------------------
water_areas <- read_sf(here('data/cleaned/la_county_waterareas.geojson'))


## ----map_water_areas------------------------------------------------------------
ggplot() +
  geom_sf(data=water_areas)


## ----get_la_river---------------------------------------------------------------
la_river <- read_sf(here('data/cleaned/los_angeles_river.geojson'))


## ----map_la_river---------------------------------------------------------------
ggplot() +
  geom_sf(data=la_river)


## ----get_la_county_boundary-----------------------------------------------------
la_county <- read_sf(here('data/cleaned/los_angeles_county/los_angeles_county.shp'))


## ----get_all_la_county_fires----------------------------------------------------
fires_all_la <- read_sf(here('data/cleaned/cal_fire_los_angeles_county.geojson'))

dim(fires_all_la)


## ----get_fires_for_10_year------------------------------------------------------
decade_fires <- fires_all_la |>
  filter(YEAR_ >= 2015)

dim(decade_fires)


## ----create_map_for_fires_for_10_years------------------------------------------
ggplot() +
  geom_sf(data=la_county) +
  geom_sf(data=decade_fires, fill='yellow')


## ----create_location------------------------------------------------------------
point <- st_point(c(-118.809407, 34.089205))
location <- st_sfc(point, crs=st_crs(4326))

location


## ----map_location---------------------------------------------------------------
mapview(location)


## ----check_crs_location_fire----------------------------------------------------
st_crs(location) == st_crs(fires_all_la)


## ----update_crs_location_fire---------------------------------------------------
fires_all_la <- st_transform(fires_all_la, crs=4326)

st_crs(location) == st_crs(fires_all_la)


## ----find_fires_for_location----------------------------------------------------

fires_for_location <- st_filter(fires_all_la, location)

fires_for_location


## ----creat----------------------------------------------------------------------
mapview(fires_for_location, zcol="FIRE_NAME") +
  mapview(location)


## ----get_damaged_structures-----------------------------------------------------
DINS_la <-read_sf(here('data/cleaned/DINS_los_angeles_county.geojson'))

dim(DINS_la)


## ----get_damaged_structures_2025------------------------------------------------
recent_DINS <- DINS_la |>
  mutate(year = year(INCIDENTST)) |>
  filter(year == 2025)

dim(recent_DINS)


## ----create_map_for_damaged_structures_2025-------------------------------------
ggplot() +
  geom_sf(data=la_county) +
  geom_sf(data=recent_DINS)


## ----create_map_for_nifc_firis--------------------------------------------------
NIFC_FIRIS_la <- read_sf(here('data/cleaned/NIFC_FIRIS_los_angeles_county.geojson'))

ggplot() +
  geom_sf(data=la_county) +
  geom_sf(data=NIFC_FIRIS_la, fill='yellow')


## ----create_map_for_2025_fires--------------------------------------------------
WFIGS_2025_la <- read_sf(here('data/cleaned/wfigs_2025_los_angeles_county.geojson'))

ggplot() +
  geom_sf(data=la_county) +
  geom_sf(data=WFIGS_2025_la, fill='yellow')


## ----get_pna_data---------------------------------------------------------------
la_county_pna <- read_sf(here('data/cleaned/LA_County_PNA_Demographics.geojson'))


## ----map_of_median_income-------------------------------------------------------
mapview(la_county_pna,
        zcol='householdincome_medhinc_cy')


## ----fix_map--------------------------------------------------------------------

la_county_pna_map <- la_county_pna |>
  select(STUD_AR_NM, householdincome_medhinc_cy)

mapview(la_county_pna_map,
        zcol='householdincome_medhinc_cy',
        layer.name ='Avg income')


## ----get_ejsm-------------------------------------------------------------------
ejsm <- read_sf(here('data/raw/EJSM_Scores-shp/6cbc6914-690f-48ec-a54f-2649a8ddb321202041-1-139ir98.m1ys.shp'))


## ----select_ejsm_fields---------------------------------------------------------
ejsm_edit <- ejsm |>
  select(CIscore, HazScore, HealthScor, SVscore, CCVscore)


## ----map_ejsm-------------------------------------------------------------------
mapview(ejsm_edit, zcol='CIscore',
        layer.name='Cumulative Impact')


## ----get_ecotopes---------------------------------------------------------------
ecotopes <- read_sf(here('data/raw/LA_Area_Ecotopes/FINAL Ecotope_Boundaries.shp'))

names(ecotopes)


## ----map_ecotopes---------------------------------------------------------------

mapview(ecotopes, zcol='ET_LndsZon')


## ----get_inaturalist_data-------------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))


## ----add_geometry_to_inaturalist------------------------------------------------
inat_sf <- st_as_sf(inat_data,
                         coords = c("longitude", "latitude"),   crs = 4326)


## ----get_indicator_species------------------------------------------------------
indicator_species <- read_csv(here('data/cleaned/LA_city_indicator_species.csv'))


## ----get_indicator_species_columns----------------------------------------------
names(indicator_species)


## ----get_scientific_names-------------------------------------------------------
indicator_scientific_names <- indicator_species$'scientific name'

indicator_scientific_names


## ----get_indicator_species_observations-----------------------------------------
indicator_sf <- inat_sf |>
  filter(scientific_name %in% indicator_scientific_names) |>
  select(scientific_name, common_name)

dim(indicator_sf)


## ----map_indicator_species------------------------------------------------------

mapview(indicator_sf)


## ----get_inaturalist_data_2-----------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))


## ----add_geometry_to_inaturalist_2----------------------------------------------
inat_sf <- st_as_sf(inat_data,
                         coords = c("longitude", "latitude"),   crs = 4326)



## ----get_native_plants_datas----------------------------------------------------
calscape_la_county <- read_csv(here("data/raw/calscape - Los Angeles County, CA.csv"))

names(calscape_la_county)


## ----get_calscape_columns-------------------------------------------------------
names(calscape_la_county)


## ----get_plants_scientific_names------------------------------------------------
plants_scientific_names <- calscape_la_county$'Botanical Name'


## ----get_native_plants_observations---------------------------------------------
native_plants_sf <- inat_sf |>
  filter(scientific_name %in% plants_scientific_names) |>
  select(scientific_name, common_name, establishment_means)

dim(native_plants_sf)


## ----map_native_plants----------------------------------------------------------

mapview(native_plants_sf)

