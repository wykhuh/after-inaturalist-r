library(readr) # read and write tabular data
library(dplyr) # manipulate data
library(ggplot2) # create data visualizations
library(lubridate) # manipulate dates
library(here) # file paths


# ## Exercise 1
# 
# Create a chart with all your observations for each year
# 
# -   Use `read_csv()` to read iNaturalist file.
# -   Use `mutate()` and `year()` to add year column
# -   Use `filter()` to select observations you made. Use 'quantron' as the user if you don't have CNC observations.
# -   Save the data frame to `my_obs_by_year` object
# -   Use `ggplot()` to set the data and aesthetics.
# -   Choose which type of chart you want: bar or line.
# 


# ## Exercise 2: Customizing a plot
# 
# Take the `my_obs_by_year` data frame from exercise 1 and create a plot. Customize the appearance or the chart. Here are some ideas.
# 
# -   add a title
# -   choose the one of the built in themes
# -   change the axis titles
# -   change the colors of the bars or lines
# 
