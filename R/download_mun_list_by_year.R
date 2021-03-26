load_bfs_mun_list <- function(date_or_year) {
  # FIXME: filtering by canton not easily possible, since the canton is not part of the downloaded table
  #        but AFAIK there are rules that determine the ranges for the mun_ids per canton; maybe we could use that?
  #
  # cf. bfs.admin.ch/bfs/de/home/dienstleistungen/forschung/api/api-gemeinde.assetdetail.15224054.html
  # if `date_or_year` is given as year, it needs to be translated to a from-to period;
  # we then choose <year>-01-01 for both;
  # also if `date_or_year` is given as date, we choose the same date for the start
  # and the end of the query-period

  if (is.numeric(date_or_year) | nchar(date_or_year) == 4) {
    # FIXME: explicit error in case year is not like an integer?
    date <- paste0("01-01-", as.character(as.integer(date_or_year)))
  } else {
    date <- format(as.Date(date_or_year), "%d-%m-%Y")
  }

  readr::read_csv(
    glue::glue("https://sms.bfs.admin.ch/WcfBFSSpecificService.svc/AnonymousRest/communes/snapshots?useBfsCode=true&startPeriod={date}&endPeriod={date}"),
    col_types = readr::cols()
    ) %>%
    # Level 3 are municipalities
    filter(Level == 3) %>%
    select(mun_id = Identifier, mun_name = Name_de) %>%
    arrange(mun_id)
}
