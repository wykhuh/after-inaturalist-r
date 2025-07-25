processFile = function(filepath) {
  original_text  <- readLines(filepath)
  temp_text <- '&&&'

  clean_text <- original_text |> gsub(pattern = "^#\\| message: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| warning: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| error: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| eval: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^#\\| include: false", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^# ", replace = '', perl = FALSE) |>
    gsub(pattern = " +$", replace = '', perl = FALSE) |>
    gsub(pattern = "^classroom_organization.*?$", replace = temp_text, perl = FALSE) |>
    gsub(pattern = "^classroom_repos.*?$", replace = temp_text, perl = FALSE) |>
    gsub(pattern = '../scripts/data_utils.R', replace = 'data_utils.R', perl = FALSE)


  # remove blank lines with readLines https://stackoverflow.com/a/11866046
  clean_text <- clean_text[which(clean_text!=temp_text)]

  # delete the lines if they contain exercise code
  exercise_code <- FALSE
  keep_lines <- c()
  for(line in clean_text) {
    # detect lines that start code snippets
    if (exercise_code & startsWith(line, '## ----')) {
      exercise_code <- FALSE
    }

    # detect lines that start exercise code snippets
    if (startsWith(line, '## ----exercise')) {
      exercise_code <- TRUE
    }

    # keep lines that aren't exercise code or has exercise label
    if(!exercise_code | startsWith(line, '## ----exercise')) {
      len <- length(keep_lines)
      keep_lines[len+1] <- line
    }

    # add two blanks lines after exercise label
    if (startsWith(line, '## ----exercise')) {
      len <- length(keep_lines)
      keep_lines[len+1] <- ''
      keep_lines[len+2] <- ''
    }
  }


  writeLines(keep_lines, con=filepath)
}



files <- c(
           # 'intro-data-analysis',
           # 'intro-r-rstudio',
           'working-with-data',
           'understanding-data',
           'creating-maps',
           'creating-charts',
           'higher-taxa',
           'other-datasets',
           'example-analysis',
           'normalizing-inat',
           'additional-analysis'
           )

# files <- c('creating-charts')


# delete files https://stackoverflow.com/a/65831178
unlink("lessons-export-r/*", recursive = TRUE, force = TRUE)

count <- 1
for (file in files) {
  input_file <- paste0('lessons/',file,'.qmd')
  output_file <- paste0('lessons-export-r/', count, '_', file,'.R')

  knitr::purl(input = input_file, output = output_file, documentation = 1)
  processFile(output_file)
  count <- count + 1
}

exercise_files <- c( 'working-with-data',
           'creating-maps',
           'creating-charts')

count <- 1
for (file in exercise_files) {
  input_file <- paste0('lessons/',file,'.qmd')
  output_file <- paste0('lessons-export-r/', count, '_', file,'-full.R')

  # knitr::purl(input = input_file, output = output_file, documentation = 2)
  count <- count + 1
}


