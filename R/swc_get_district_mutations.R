swc_get_district_mutations <- function() {
  readr::read_csv(csv_file("district_mutations"))
}
