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
