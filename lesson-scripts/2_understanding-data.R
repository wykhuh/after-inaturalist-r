## ----loading_packages---------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(here) # file paths
library(tibble) # tibbles are updated version of dataframes


## ----assign_read_csv_to_object------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))


## ----call_glimpse-------------------------------------------------------------
glimpse(inat_data)


## ----adding_numbers-----------------------------------------------------------
1 + 2


## ----adding_words-------------------------------------------------------------
"cat" + "dogs"


## ----numeric_vector-----------------------------------------------------------
numbers <- c(1, 2, 5)
numbers

class(numbers)


## ----character_vector---------------------------------------------------------
characters <- c("apple", 'pear', "grape")
characters

class(characters)


## ----logical_vector-----------------------------------------------------------
logicals <- c(TRUE, FALSE, TRUE)
logicals

class(logicals)


## ----vector_conversion--------------------------------------------------------
mixed <- c(1, "apple", TRUE)
mixed

class(mixed)


## ----create_dataframe---------------------------------------------------------
df <- data.frame(Numbers = numbers, Characters = characters)
df


## ----class_of_dataframe-------------------------------------------------------
class(df)


## ----create_tibble------------------------------------------------------------
tb <- tibble(Numbers = numbers, Characters = characters)
tb


## ----class_of_tibble----------------------------------------------------------
class(tb)


## ----class_of_inat_data-------------------------------------------------------
class(inat_data)


## ----c-vector-na--------------------------------------------------------------
numbers <- c(1, 2, NA)
numbers

class(numbers)


## ----min----------------------------------------------------------------------
min(numbers)


## ----min_na_rm----------------------------------------------------------------
min(numbers,  na.rm = TRUE)

