# add_inat_count_to_boundary_sf() counts the number of iNaturalist observations
# per boundary for a given  `sf` object with multiple boundaries, and adds a
# `observations_count` column to the boundaries `sf` object.

# Arguments
# inat_sf: `sf` object with iNaturalist observations
# boundaries_sf: `sf` object that has multiple boundaries
# boundaries_field: field in boundaries_sf that has unique values such as ID
# or boundary name
add_inat_count_to_boundary_sf <- function(inat_sf, boundaries_sf, boundaries_field) {

  boundaries_basic_sf <- boundaries_sf %>%
    select(!!as.symbol(boundaries_field))

  # We want to figure out how many observations are in each boundary.
  # `st_join()` from sf figures out if a spatial object touch, cross, or is
  # within a second spatial object. If an first item intersects the second item,
  # then the columns from the second item are added to the first item.

  # The following code will figure out if an observation in `inat_sf`
  # intersects the boundaries in `boundaries_basic_sf`. If the observation
  # intersects a boundary, the boundaries_field is added the observation.
  # If the observation is not inside a boundary, boundaries_field is set to `NA`.

  inat_boundaries_sf <- st_join(inat_sf, boundaries_basic_sf)

  # Now that we have added the boundaries_field to each observation,
  # we can use the `count()` function to count the number of observations per
  # boundary. Use `st_drop_geometry()` to dropping the `geometry` column.

  boundaries_count <- inat_boundaries_sf %>%
    st_drop_geometry() %>%
    count(!!(as.symbol(boundaries_field)), name='observations_count')

  # Next use `left_join()` from dplyr to add `observations_count` from
  # `boundaries_count` to `boundaries_sf`. `left_join()`
  # will use the boundaries_field to combine the data.

  left_join(boundaries_sf, boundaries_count, by=boundaries_field)
}

# download_inaturalist_images downloads all the `image_url` for dataframe of
# an iNaturalist observations. The images will be saved in the results directory.
# A new directory will be created for each `scientific_name`. The image name
# will contain the scientific name, observation id, username and license.

# Arguments
# inat_df: dataframe with iNaturalist observations
download_inaturalist_images <- function(inat_df) {
  download_row_image <- function(row) {
    url = row['image_url']
    taxa = row['scientific_name']
    id = row['id']
    user = row['user_login']
    license = row['license']

    # create filename
    filename = paste0(taxa, '_', id, '_', user, '_', license, '.jpg')
    print(filename)

    # create directories
    dir.create(here('results/images'), showWarnings = FALSE)
    dir.create(here('results/images', taxa), showWarnings = FALSE)

    # download the image
    tryCatch({
      download.file(url, here('results/images', taxa, filename))
    },
    error = function(e){
      message('Caught an error!')
      print(e)
    })
  }

  apply(inat_df, 1, download_row_image)
}
