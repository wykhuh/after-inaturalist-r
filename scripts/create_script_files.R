processFile = function(filepath) {
  original_text  <- readLines(filepath)
  temp_text <- '&&&'

  clean_text <- original_text |> gsub(pattern = "^#\\| message: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| warning: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| error: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| eval: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^# ", replace = '', perl = FALSE) |>
    gsub(pattern = '../scripts/map_utils.R', replace = 'map_utils.R', perl = FALSE)


  # remove blank lines with readLines https://stackoverflow.com/a/11866046
  clean_text <- clean_text[which(clean_text!=temp_text)]

  writeLines(clean_text, con=filepath)

}

files <- c('additional-analysis')


files <- c('intro-data-analysis',
           'intro-r-rstudio',
           'working-with-data',
           'understanding-data',
           'creating-maps',
           'creating-charts',
           'other-datasets',
           'additional-analysis')

# delete files https://stackoverflow.com/a/65831178
unlink("lesson-scripts/*", recursive = TRUE, force = TRUE)


for (file in files) {
  input_file <- paste0('lessons/',file,'.qmd')
  output_file <- paste0('lesson-scripts/',file,'.R')

  knitr::purl(input = input_file, output = output_file, documentation = 1)
  processFile(output_file)
}

