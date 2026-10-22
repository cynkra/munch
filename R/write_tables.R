# Municipality splits existed before 2005
write_all_mapping_tables <- function(source_year = 2005) {
  MAX_YEAR <- lubridate::year(
    max(swc_get_municipality_mutations()$mAdmissionDate, na.rm = TRUE)
  )

  years <- seq(source_year, MAX_YEAR, by = 1)

  compact <- map(years, write_mapping_table, source_year = source_year, type = "compact")
  flat <- map(years, write_mapping_table, source_year = source_year, type = "flat")

  invisible()
}

write_mapping_table <- function(source_year, year, type) {
  message(type, ":", year)
  mapping <- swc_get_merger_mapping_table(source_year, year, type = type)

  path <- new_csv_file(file.path(type, year))
  dir.create(dirname(path), showWarnings = FALSE)
  readr::write_csv(mapping, path)

  tibble(year, type, path, nrow = nrow(mapping))
}
