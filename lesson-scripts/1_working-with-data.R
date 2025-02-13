## ----loading_packages---------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(lubridate) # manipulate dates
library(here) # file paths
library(stringr) # work with string


## ----assign_read_csv_to_object------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv'))


## ----call_glimpse-------------------------------------------------------------
glimpse(inat_data)


## ----head---------------------------------------------------------------------
head(inat_data)


## ----tail---------------------------------------------------------------------
tail(inat_data)


## ----view_dataframe-----------------------------------------------------------
View(inat_data)


## ----use_names_to_list_fields-------------------------------------------------
names(inat_data)


## ----use_dim_to_get_size------------------------------------------------------
dim(inat_data)


## ----function_help_documentation----------------------------------------------
?head


## ----named_arguments_in_order-------------------------------------------------
head(x = inat_data, n = 10)


## ----arguments_in_order-------------------------------------------------------
head(inat_data, 10)


## ----name_arguments_out_of_order----------------------------------------------
head(n = 10, x = inat_data)


## ----select_columns-----------------------------------------------------------
select(inat_data, user_login, common_name, scientific_name, observed_on)


## ----filter_rows--------------------------------------------------------------
filter(inat_data, common_name == 'Western Fence Lizard')



## ----filter_rows_scientific_name----------------------------------------------
filter(inat_data, scientific_name == 'Sceloporus occidentalis')



## -----------------------------------------------------------------------------
oaks <- filter(inat_data, common_name == 'oaks')


## -----------------------------------------------------------------------------
quercus <- filter(inat_data, scientific_name == 'Quercus')


## ----pipe_filter_select-------------------------------------------------------
inat_data %>%
  filter(user_login == 'natureinla') %>%
  select(user_login, common_name, scientific_name, observed_on)


## ----coordinates_obscured-----------------------------------------------------

inat_data$coordinates_obscured


## ----call_table---------------------------------------------------------------
table(inat_data$coordinates_obscured)


## ----observations_with_unobscured_coordinates---------------------------------
inat_data %>%
  filter(coordinates_obscured == FALSE) %>%
  select(user_login, common_name, scientific_name, observed_on)


## ----select_filter_error------------------------------------------------------

inat_data %>%
  select(user_login, common_name, scientific_name, observed_on)  %>%
  filter(coordinates_obscured == FALSE)


## ----quality_grade_values-----------------------------------------------------
unique(inat_data$quality_grade)


## ----research_grade_observations----------------------------------------------
inat_data %>%
  filter(quality_grade == 'research')  %>%
  select(user_login, common_name, scientific_name, observed_on)


## ----error_typo_pipe----------------------------------------------------------
inat_data %>
  select(user_login, observed_on, common_name)


## ----error_misspell_field-----------------------------------------------------
inat_data %>%
  select(user_logi, observed_on, common_name)


## ----error_equality-----------------------------------------------------------
inat_data %>%
  filter(user_login = 'natureinla')


## ----error_parenthesis--------------------------------------------------------
inat_data %>%
  select(user_login, observed_on, common_name))


## ----exercise_your_observations-----------------------------------------------


## ----filter_with_common_name_and_quality_grade--------------------------------
my_data <- inat_data %>%
  filter(common_name == 'Western Fence Lizard' &
           quality_grade == 'research')  %>%
  select(user_login, common_name, scientific_name, observed_on, quality_grade)


## ----view_my_data-------------------------------------------------------------
View(my_data)


## ----and_unique_common_name---------------------------------------------------
unique(my_data$common_name)


## ----and_unique_quality_grade-------------------------------------------------
unique(my_data$quality_grade)


## ----get_summary_of_unobscured_observations-----------------------------------
my_data <- inat_data %>%
  filter(coordinates_obscured == FALSE)


summary(my_data$positional_accuracy)


## ----filter_with_coordinates_obscured_and_positional_accuracy-----------------
my_data <- inat_data %>%
  filter(coordinates_obscured == FALSE &
           positional_accuracy <= 5) %>%
  select(user_login, common_name, scientific_name, positional_accuracy, coordinates_obscured)

dim(my_data)


## ----and_unique_coordinates_obscured------------------------------------------
unique(my_data$coordinates_obscured)


## ----and_unique_positional_accuracy-------------------------------------------
unique(my_data$positional_accuracy)


## ----filter_with_or_2_species-------------------------------------------------
my_data <- inat_data %>%
  filter(common_name == 'Western Honey Bee' |
        common_name == 'Western Fence Lizard')  %>%
  select(user_login, observed_on, common_name)

dim(my_data)


## ----or_common_name-----------------------------------------------------------
unique(my_data$common_name)


## ----and_comparison-----------------------------------------------------------
and_data <- inat_data %>%
  filter(user_login == 'natureinla' &
           common_name == 'Western Fence Lizard')

dim(and_data)


## ----and_comparison_user_login------------------------------------------------
unique(and_data$user_login)


## ----and_comparison_common_name-----------------------------------------------
unique(and_data$common_name)


## ----or_comparison------------------------------------------------------------
or_data <- inat_data %>%
  filter(user_login == 'natureinla' |
           common_name == 'Western Fence Lizard')

dim(or_data)


## ----or_comparison_user_login-------------------------------------------------
unique(or_data$user_login) %>% length


## ----or_comparison_common_name------------------------------------------------
unique(or_data$common_name) %>% length


## ----create_vector------------------------------------------------------------
c(1, 2, 5)


## ----in_vector----------------------------------------------------------------
1 %in% c(1, 2, 5)
3 %in% c(1, 2, 5)


## ----view_license_values------------------------------------------------------
table(inat_data$license)


## ----filter_by_license--------------------------------------------------------
my_data <- inat_data %>%
  filter(license %in% c('CC0', 'CC-BY', 'CC-BY-NC')) %>%
  select(user_login, observed_on, common_name, license)

dim(my_data)



## ----unique_license-----------------------------------------------------------
unique(my_data$license)



## ----exercise_your_research_grade---------------------------------------------


## ----get_common_names---------------------------------------------------------
common_names <- unique(inat_data$common_name)

length(common_names)


## ----get_matches_for_lizard---------------------------------------------------
str_subset(common_names, pattern = 'lizard')


## ----get_matches_for_lizard_case_insensitive----------------------------------
str_subset(common_names, pattern = '(?i)lizard')


## ----get_matches_for_ants-----------------------------------------------------
str_subset(common_names, pattern = '(?i)ants')


## ----get_matches_for_word_ants------------------------------------------------
str_subset(common_names, pattern = "(?i)\\bants\\b")


## ----get_matches_for_starts_with_ants-----------------------------------------
str_subset(common_names, pattern = "(?i)\\bant")[0:30]


## ----get_matches_for_ends_with_ants-------------------------------------------
str_subset(common_names, pattern = "(?i)ant\\b")[0:30]


## ----get_observations_for_ants------------------------------------------------
ants <- c(
"Acorn Ants and Allies",
"Acrobat Ants",
"Argentine Ant",
"Big-headed Ants",
"Californicus-group Harvester Ants",
"Camponotin Ants",
"Carpenter Ants",
"Citronella Ants, Fuzzy Ants, and Allies",
"fallax-group Big-headed Ants",
"Formicine Ants",
"Furrowed Ants",
"Lasiin Ants",
"Leptomyrmecin Ants",
"Molesta-group Thief Ants",
"Myrmicine Ants",
"Pavement Ants",
"Pyramid Ants",
"Sneaking Ants",
"Sneaking Ants",
"Solenopsis Fire Ants and Thief Ants",
"Velvety Tree Ants",
"Velvety Tree Ants"
)

ants_obs <- inat_data %>%
  filter(common_name %in% ants) %>%
  select(user_login, observed_on, common_name)

dim(ants_obs)


## ----complex_queries----------------------------------------------------------
complex_query <- inat_data %>%
  filter(user_login == 'cdegroof' |
           user_login == 'deedeeflower5') %>%
  filter(common_name == 'Western Fence Lizard')  %>%
  select(user_login, common_name, scientific_name, observed_on)

dim(complex_query)


## ----complex_unique_common_name-----------------------------------------------
unique(complex_query$common_name)


## ----complex_unique_user_login------------------------------------------------
unique(complex_query$user_login)


## ----incorrect_and_or---------------------------------------------------------
alt_1 <- inat_data %>%
  filter(user_login == 'cdegroof' |
           user_login == 'deedeeflower5' &
           common_name == 'Western Fence Lizard')  %>%
  select(user_login, common_name, scientific_name, observed_on)

dim(alt_1)


## ----alt_1_unique_user_login--------------------------------------------------
unique(alt_1$user_login)


## ----alt_1_unique_common_name-------------------------------------------------
unique(alt_1$common_name) %>% length


## ----parenthesis_and_or-------------------------------------------------------
alt_2 <- inat_data %>%
  filter((user_login == 'cdegroof' | user_login == 'deedeeflower5') &
           common_name == 'Western Fence Lizard')  %>%
  select(user_login, common_name, scientific_name, observed_on)

dim(alt_2)


## ----alt_2_unique_user_login--------------------------------------------------
unique(alt_2$user_login)


## ----alt_2_unique_common_name-------------------------------------------------
unique(alt_2$common_name)


## ----exercise_unique_common_names---------------------------------------------


## ----exercise_two_species-----------------------------------------------------


## ----create_character_vector--------------------------------------------------
letters <- c('a','b','c', 'd')


## ----get_first_item-----------------------------------------------------------
letters[1]


## ----get_2nd_3rd_item---------------------------------------------------------
letters[2:3]


## ----show_observed_on---------------------------------------------------------
inat_data$observed_on[10317:10320]


## ----get_years_from_observed_on-----------------------------------------------
year(inat_data$observed_on)[10317:10320]


## ----mutate_year--------------------------------------------------------------
temp <- inat_data %>%
  mutate(year = year(observed_on))


## ----counts_per_year----------------------------------------------------------
table(temp$year)


## ----year_class---------------------------------------------------------------
class(temp$year)


## ----2020_observations--------------------------------------------------------
temp <- inat_data %>%
  mutate(year = year(observed_on)) %>%
  filter(year == 2020)


## ----unique_years-------------------------------------------------------------
unique(temp$year)


## ----2018_2020_observations---------------------------------------------------
temp <- inat_data %>%
  mutate(year = year(observed_on)) %>%
  filter(year >= 2018 & year <= 2020)


## ----unique_years_multiple----------------------------------------------------
unique(temp$year)


## ----exercise_last_year-------------------------------------------------------


## ----count_year---------------------------------------------------------------
inat_data %>%
  mutate(year = year(observed_on)) %>%
  count(year)


## ----rename_count_column------------------------------------------------------
inat_data %>%
  mutate(year = year(observed_on)) %>%
  count(year, name='obs_count')


## ----count_species------------------------------------------------------------
counts <- inat_data %>%
  count(common_name, scientific_name, name='obs_count')

counts


## ----order_counts-------------------------------------------------------------
counts <- inat_data %>%
  count(common_name, scientific_name, name='obs_count')   %>%
  arrange(obs_count)

counts


## ----desc_count---------------------------------------------------------------
counts <- inat_data %>%
  count(common_name, scientific_name, name='obs_count') %>%
  arrange(desc(obs_count))

counts


## ----top_ten------------------------------------------------------------------
counts <- inat_data %>%
  count(common_name, scientific_name, name='obs_count') %>%
  arrange(desc(obs_count))  %>%
  slice(1:10)

counts


## ----kingdoms_count-----------------------------------------------------------
counts <- inat_data %>%
  count(taxon_kingdom_name, name='obs_count') %>%
  arrange(desc(obs_count))

counts


## ----animal_phylums-----------------------------------------------------------
counts <- inat_data %>%
  filter(taxon_kingdom_name == 'Animalia') %>%
  count(taxon_phylum_name, name='obs_count') %>%
  arrange(desc(obs_count))

counts


## ----exercise_observations_per_year-------------------------------------------


## ----3_condition_my_observation-----------------------------------------------

my_obs <- inat_data %>%
  filter(user_login == 'natureinla' &
           quality_grade == 'research' &
           coordinates_obscured == FALSE)

my_obs



## ----save_file----------------------------------------------------------------
write_csv(my_obs, here('data/cleaned/my_observations.csv'), na='')

