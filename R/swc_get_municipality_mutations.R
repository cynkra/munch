swc_get_municipality_mutations <- function() {
  as_tibble(readr::read_csv(csv_file("mut/municipality_mutations"), col_types = "iiciccciicDicDD"))
}
