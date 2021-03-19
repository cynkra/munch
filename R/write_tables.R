# Municipality splits existed 2005 and before
write_all_mapping_tables <- function(source_year = 2006) {
  MAX_YEAR <- lubridate::year(
    max(swc_get_municipality_mutations()$mAbolitionDate, na.rm = TRUE)
  ) + 1L

  years <- seq(source_year, MAX_YEAR, by = 1)

  compact <- map(years, write_mapping_table, source_year = source_year, type = "compact")
  flat <- map(years, write_mapping_table, source_year = source_year, type = "flat")

  invisible()
}

write_mapping_table <- function(source_year, year, type) {
  message(year)
  mapping <- swc_get_merger_mapping_table(source_year, year, type = type)

  path <- new_csv_file(file.path(type, year))
  dir.create(dirname(path), showWarnings = FALSE)
  readr::write_csv(mapping, path)

  tibble(year, type, path, nrow = nrow(mapping))
}
