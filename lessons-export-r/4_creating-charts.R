## ----load_packages--------------------------------------------------------------

library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(ggplot2) # create data visualizations
library(lubridate) # manipulate dates
library(here) # file paths


## ----load_inaturalist_data------------------------------------------------------
inat_data <- read_csv(here('data/cleaned/cnc-los-angeles-observations.csv.zip'))



## ----add_year_column------------------------------------------------------------
inat_year <- inat_data |>
  mutate(year = year(observed_on))


## ----pass_data_to_ggplot--------------------------------------------------------
ggplot(data = inat_year)


## ----pass_aes_to_ggplot---------------------------------------------------------
ggplot(data = inat_year, mapping = aes(x = year))


## ----set_bar_type---------------------------------------------------------------
ggplot(data = inat_year,  mapping = aes(x = year)) +
  geom_bar()



## ----horizontal_bar_chart-------------------------------------------------------
ggplot(data = inat_year, mapping = aes(y = year)) +
  geom_bar()


## ----create_dataframe_with_year_count-------------------------------------------
inat_year_count <- inat_year |>
  count(year, name='count')

inat_year_count


## ----create_line_chart----------------------------------------------------------
ggplot(data = inat_year_count,
       mapping = aes(x = year, y=count)) +
  geom_line()


## ----create_line_point_chart----------------------------------------------------
ggplot(data = inat_year_count,
       mapping = aes(x = year, y=count)) +
  geom_line() +
  geom_point()



## ----create_column_chart--------------------------------------------------------
ggplot(data = inat_year_count,
       mapping = aes(x = year, y = count)) +
  geom_col()


## ----create_stacked_bar_chart---------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar()


## ----exercise_create_bar_chart--------------------------------------------------


## ----get_all_colors-------------------------------------------------------------
colors()


## ----set_bar_color--------------------------------------------------------------
ggplot(data = inat_year, mapping = aes(x = year)) +
  geom_bar(fill='aquamarine', color='black')


## ----set_bar_color_using_hex_code-----------------------------------------------
ggplot(data = inat_year,  mapping = aes(x = year)) +
  geom_bar(fill='#75cd5e')


## ----set_bar_and_point_color----------------------------------------------------
ggplot(data = inat_year_count,
       mapping = aes(x = year, y=count)) +
  geom_line(color='#75cd5e') +
  geom_point(color='blue')


## ----stacked_bar_chart_default_colors-------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar()


## ----stacked_bar_chart_viridis--------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar() +
  scale_fill_viridis_d()


## ----stacked_bar_chart_viridis_error--------------------------------------------

ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar() +
  scale_fill_viridis_c()


## ----stacked_bar_chart_brewer---------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar() +
  scale_fill_brewer()


## ----stacked_bar_chart_manual---------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar() +
  scale_fill_manual(values=c("#DADAEB", "#9E9AC8", "#6A51A3"))



## ----create_chart_with_custom_breaks--------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year)) +
  geom_bar()  +
  scale_fill_viridis_d() +
  scale_x_continuous(n.breaks=8) +
  scale_y_continuous(n.breaks=7)


## ----assign_plot_to_object------------------------------------------------------
myplot <- ggplot(data = inat_year,
                 mapping = aes(x = year)) +
  geom_bar(fill='#75cd5e') +
  scale_x_continuous(n.breaks = 8)


  myplot


## ----set_theme, warning=FALSE---------------------------------------------------
myplot +
  theme_bw()


## ----rotate_axis_label----------------------------------------------------------
myplot +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))



## ----set_text_size, warning=FALSE-----------------------------------------------
myplot +
  theme_bw() +
  theme(axis.title = element_text(size = 14))


## ----remove_vertical_grid_lines, warning=FALSE----------------------------------
myplot +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


## ----changing_labels------------------------------------------------------------
myplot +
  labs(title = "CNC Los Angeles",
       subtitle="Observations per year",
       x = "Year",
       y = "Observations")


## ----changing_legend_title------------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year, fill = quality_grade)) +
  geom_bar() +
  labs(fill = "Quality Grade")


## ----exercise_change_chart_appearance-------------------------------------------


## ----create_facets--------------------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year)) +
  geom_bar() +
  facet_wrap(vars(quality_grade))


## ----create_facets_1_column-----------------------------------------------------
ggplot(data = inat_year,
       mapping = aes(x = year)) +
  geom_bar() +
  facet_wrap(vars(quality_grade), ncol = 1)


## ----save_plot------------------------------------------------------------------

# create plot
finalplot <- myplot +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 20)) +
  labs(title = "CNC Los Angeles",
       subtitle="Observations per year",
       x = "Year",
       y = "Observations")

# save plot
ggsave(filename = here('results/observations_per_year.jpg'), plot = finalplot,
       height = 6, width = 8)

