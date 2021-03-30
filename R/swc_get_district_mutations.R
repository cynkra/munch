swc_get_district_mutations <- function() {
  readr::read_csv(csv_file("mut/district_mutations"), col_types = "iiiccfifDifDD")
}
