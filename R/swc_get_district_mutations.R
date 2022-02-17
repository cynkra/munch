swc_get_district_mutations <- function() {
  as_tibble(readr::read_csv(csv_file("mut/district_mutations"), col_types = "iiicccicDicDD", lazy = FALSE))
}
