library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(ggplot2) # create data visualizations
library(sf) # handle vector geospatial data
library(mapview) # create interactive maps
library(here) # file paths
source(here('scripts/data_utils.R'))


# ## Exercise 1
# 
# Create a map for one species.
# 
# -   use `read_csv()` to read iNaturalist file. Assign the results to `my_inat_data` object.
# -   use `st_as_sf()` to add `geometry` column.
# -   use `select()` to pick four columns.
# -   use `filter()` to select observations for one species.
# -   Assign the results of `filter()` and `select()` to `my_inat_sf`
# -   create either a static or interactive map.
# 


# ## Exercise 2
# 
# Create a map for one species with LA County boundary.
# 
# -   use iNaturalist observations `my_inat_sf` from Exercise 1
# -   use `read_sf()` to read LA County boundary
# -   check if iNaturalist and LA County boundary use the same CRS
# -   create either a static or interactive map.
# 


# ## Exercise 3
# 
# Create a map for all observations that are inside of a specific area
# 
# -   use `my_inat_data` from exercise 1 to access iNaturalist data
# -   use `st_as_sf()` to add `geometry` column to iNaturalist data.
# -   use `select()` to select 4 columns for iNaturalist data.
# -   use [Draw map boundaries](https://wykhuh.github.io/draw-map-boundaries/) to draw and download an area that you are interested in.
# -   Save the file to the `data/raw` directory.
# -   use `read_sf()` to read your boundary data.
# -   check if iNaturalist observations and your boundary use the same CRS
# -   get observations inside a boundary
# -   create static or interactive map
# 
