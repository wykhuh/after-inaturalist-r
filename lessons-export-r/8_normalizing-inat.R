## ----load_packages--------------------------------------------------------------

library(readr)
library(dplyr)
library(here)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)


## ----fix_sf_bug_1---------------------------------------------------------------
sf_use_s2(FALSE)


## ----get_inaturalist_data-------------------------------------------------------
allobs <- read_csv(here("data/cleaned/cnc-los-angeles-observations.csv.zip"))



## ----get_research_observations--------------------------------------------------
allobs_research <- allobs |>
  filter(quality_grade=="research")



## ----add_geometry_column--------------------------------------------------------
allobs_sf <- allobs_research |>
  st_as_sf(coords = c("longitude", "latitude"),   crs = 4326)


## ----get_hollywood_boundary-----------------------------------------------------
hollywood_boundary <- read_sf(here('data/raw/Hollywood Boundary/hollywood_boundary.shp'))



## ----check_crs------------------------------------------------------------------
st_crs(hollywood_boundary) == st_crs(allobs_sf)



## ----update_hollywood_crs-------------------------------------------------------
hollywood_boundary <- st_transform(hollywood_boundary, crs = st_crs(allobs_sf))

st_crs(hollywood_boundary) == st_crs(allobs_sf)



## ----get_observations_inside_hollywood------------------------------------------
neighborhood_obs <- st_filter(allobs_sf, hollywood_boundary)


## ----map_observations_in_hollywood----------------------------------------------
mapview(neighborhood_obs) +
  mapview(hollywood_boundary)



## ----add_year_to_inaturalist_observations---------------------------------------
neighborhood_obs <- neighborhood_obs |>
  mutate(year = year(observed_on))



## ----count_observations_by_year-------------------------------------------------
table(neighborhood_obs$year)


## ----get_minimun_observations_per_year------------------------------------------
lowest_year <- min(table(neighborhood_obs$year))

lowest_year


## ----get_random_observations_per_year-------------------------------------------
random_obs_per_year <- neighborhood_obs |>
  st_drop_geometry() |>
  group_by(year) |>
  slice_sample(n = lowest_year, replace=FALSE) |>
  select(year, scientific_name)

head(random_obs_per_year)


## ----count_random_observations_per_yeare----------------------------------------
table(random_obs_per_year$year)


## ----get_distinct_species_per_year----------------------------------------------
unique_species_per_year <- distinct(random_obs_per_year)


## ----count_species_per_year-----------------------------------------------------
richness_dataframe <- unique_species_per_year |>
  count(year, name='richness')

richness_dataframe


## ----plot_richness_per_year-----------------------------------------------------
ggplot(data = richness_dataframe, mapping = aes(x=year, y=richness)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.title = element_text(size =14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Species Richness Over Time in Hollywood",
       x = "Year",
       y = "Species Richness")

