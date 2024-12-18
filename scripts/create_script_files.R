processFile = function(filepath) {
  original_text  <- readLines(filepath)
  print(original_text)

  original_text |> gsub(pattern = "^#\\| message: false", replace = "", perl = FALSE) |>
    gsub(pattern = "^#\\| warning: false", replace = "", perl = FALSE) |>
    gsub(pattern = "^#\\| error: false", replace = "", perl = FALSE) |>
    gsub(pattern = "^#\\| cache: true", replace = "", perl = FALSE) |>
    writeLines(con=filepath)

}

input_file <- "lessons/working-with-data.qmd"
output_file <- "scripts/working-with-data.R"

input_file <- "lessons/census.qmd"
output_file <- "scripts/census.R"



knitr::purl(input = input_file, output = output_file,documentation = 1)

processFile(output_file)
