write_all_mapping_tables <- function() {
  SOURCE_YEAR <- 2005
  MAX_YEAR <- lubridate::year(
    max(swc_get_municipality_mutations()$mAbolitionDate, na.rm = TRUE)
  ) + 1L

  years <- seq(SOURCE_YEAR, MAX_YEAR, by = 1)

  compact <- map(years, write_mapping_table, source_year = SOURCE_YEAR, type = "compact")
  flat <- map(years, write_mapping_table, source_year = SOURCE_YEAR, type = "flat")

  invisible()
}

write_mapping_table <- function(source_year, year, type) {
  message(year)
  mapping <- swc_get_merger_mapping_table(source_year, year, type = type, canton = "TI")

  path <- new_csv_file(file.path(type, year))
  dir.create(dirname(path), showWarnings = FALSE)
  readr::write_csv(mapping, path)

  tibble(year, type, path, nrow = nrow(mapping))
}
