input_file <- "lessons/working-with-data.qmd"
output_file <- "scripts/working-with-data.R"




processFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    clean_line = gsub("#'", '',line)
    print(clean_line)
  }

  close(con)
}


knitr::purl(input = input_file, output = output_file,documentation = 0)

#processFile(output_file)
