source_folder <- function(subfolder) {
  files <- list.files(here(subfolder), pattern = "\\.R$", full.names = TRUE)
  
  for (file in files) {
    message("Sourcing file: ", file)
    source(file)
  }