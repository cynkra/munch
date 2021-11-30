load_bfs_mun_list <- function(date_or_year = lubridate::year(Sys.Date()), canton = NULL) {
  # cf. https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/forschung/api/api-gemeinde.assetdetail.15224054.html
  date <- date_or_year_to_date(date_or_year)
  all_data <- readr::read_csv(
    glue::glue("https://sms.bfs.admin.ch/WcfBFSSpecificService.svc/AnonymousRest/communes/snapshots?useBfsCode=true&startPeriod={date}&endPeriod={date}"),
    col_types = "icciicccccccccdddddddddddddddlccll"
    )
    cantons <- all_data %>%
      # Level 1 are cantons
      filter(Level == 1) %>%
      select(ct_id = Identifier, ct_name = Name_de, ct_short = ABBREV_1_Text_de)
    districts <- all_data %>%
      # Level 2 are districts
      filter(Level == 2) %>%
      select(dist_id = Identifier, ct_id = Parent)
    # Level 3 are municipalities
    res <- all_data %>%
      filter(Level == 3) %>%
      select(mun_id = Identifier, mun_name = Name_de, dist_id = Parent) %>%
      left_join(districts, by = "dist_id") %>%
      left_join(cantons, by = "ct_id") %>%
      select(mun_id, mun_name, ct_short, ct_name) %>%
      arrange(mun_id)
    if (!is.null(canton)) {
      res <- filter(res, ct_short == canton)
    }
    res
}

date_or_year_to_date <- function(date_or_year) {
  # if `date_or_year` is given as year, it needs to be translated to a from-to period;
  # we then choose <year>-01-01 for both;
  # also if `date_or_year` is given as date, we choose the same date for the start
  # and the end of the query-period
  tryCatch({
    if (is.numeric(date_or_year) || nchar(date_or_year) == 4) {
      date <- paste0("01-01-", as.character(as.integer(date_or_year)))
    } else {
      date <- format(as.Date(date_or_year), "%d-%m-%Y")
    }},
    error = function(e) abort_not_date_or_year(),
    warning = function(w) abort_not_date_or_year()
  )
  if (as.Date(date, format = "%d-%m-%Y") < as.Date("12-09-1848", "%d-%m-%Y")) abort_date_too_early(date)
  date
}
