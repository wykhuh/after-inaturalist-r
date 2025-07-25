library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(here) # file paths
library(stringr) # work with string


# ## Exercise 1
# 
# Get all your City Nature Challenge observations.
# 
# -   Use `read_csv()` to load the CNC CSV. Assign the results to `my_inat_data` object.
# -   Use `filter()` to select observations with your iNaturalist username. If you don't have any CNC observations, use 'quantron' the most prolific community scientist for CNC Los Angeles.
# -   Use `select()` to select 4 columns. One of the columns should be `common_name`
# -   assign the results of `filter()` and `select()` to `my_obs` object
# -   click on `my_obs` in the Environment tab to see the results
# 


# ## Exercise 2
# 
# Get all your observations that are research grade
# 
# -   use `my_inat_data` from Exercise 1 to access CNC observations
# -   Use `&` with `filter()` since we want to pick observations by both username and quality grade. Use 'quantron' as the user if you don't have CNC observations.
# -   Use `select()` to pick 4 columns
# 


# ## Exercise 3
# 
# Get all your observations for two species
# 
# -   Use `my_inat_data` to access CNC observations
# -   Use `unique(my_obs$common_names)` from Exercise 1 to find two species name.
# -   Use `filter(), |` to pick two species
# -   Use `filter()` to pick your username. Use 'quantron' as the user if you don't have CNC observations.
# -   Use `select()` to pick four columns.
# 


# ## Exercise 4
# 
# Get all of your observations from 2024.
# 
# -   Use `my_inat_data` to access CNC observations
# -   Use `mutate()` and `year()` to add year column
# -   Use `filter()` to pick observations with your username and year is 2024. Use 'quantron' as the user if you don't have CNC observations.
# -   Use `select()` to pick 4 columns
# 


# ## Exercise 5
# 
# Get the number of observation you made per year
# 
# -   Use `my_inat_data` to access CNC observations
# -   Use `mutate()` and `year()` to add year column
# -   Use `count()` to count the number of observations per year
# -   Use `filter()` to select observations with your username. Use 'quantron' as the user if you don't have CNC observations.
# 
