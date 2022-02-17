pkgload::load_all()

if (check_data()) {
  if (check_past_changes()) {
    if (interactive()) {
      warning(
        "Past data changed. ",
        "Please double-check carefully with `daff_municipality_mutations()` ",
        "and other `daff_*()` helpers!",
        call. = FALSE
      )
    } else {
      stop("Past data changed, please double-check manually and run data-raw/update-data.R")
    }
  }

  overwrite_data()
}
