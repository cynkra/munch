swc_get_cantons <- function() {
  readr::read_csv(csv_file("mut/canton"), col_types = "iccD")
}

csv_file <- function(name) {
  system.file("csv", paste0(name, ".csv"), package = "munch")
}

new_csv_file <- function(...) {
  paste0(file.path("inst/csv", ...), ".csv")
}
