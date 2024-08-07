#' Download municipality mutation data
#'
#' @export
swc_read_data <- function() {
  bfs_nr <- "dz-b-00.04-hgv-01"

  bfs_home <- "https://www.bfs.admin.ch"

  asset_page <- xml2::read_html(sprintf("%s/asset/de/%s", bfs_home, bfs_nr))

  asset_text <- rvest::html_text(asset_page, bfs_nr)

  asset_number <-
    asset_text %>%
    stringr::str_extract("https://.*assets/.*/") %>%
    stringr::str_extract("[0-9]+")

  pub_date <-
    asset_text %>%
    stringr::str_extract("Ver\u00f6ffentlicht am\n.*[0-9]+.[0-9]+.[0-9]+") %>%
    stringr::str_extract("[0-9]+.[0-9]+.[0-9]+")

  record_hist_url <- paste0(
    "https://www.bfs.admin.ch/bfsstatic/dam/assets/", asset_number, "/master"
  )

  zip_file_name <- tempfile(fileext = ".zip")
  logging::logdebug(zip_file_name)
  on.exit(unlink(zip_file_name), add = TRUE)

  # `mode = "wb"` needed in order for it to work on Windows
  # cf. https://github.com/tidyverse/readxl/issues/126
  download.file(record_hist_url, zip_file_name, quiet = TRUE, mode = "wb")

  unzip_dir_name <- tempfile()
  logging::logdebug(unzip_dir_name)
  on.exit(unlink(unzip_dir_name, recursive = TRUE), add = TRUE)

  all_file_list <- unzip(zip_file_name, list = TRUE)
  file_list <- all_file_list[grepl("/1[.]1/", all_file_list$Name), ]
  unzip(zip_file_name, exdir = unzip_dir_name)

  # Reading using unz() and recoding "on the fly"
  # didn't work for some reason
  ft <- list(
    canton = list(n = "KT", colnames = c(
      "cId",
      "cAbbreviation",
      "cLongName",
      "cDateOfChange"
    )),
    district = list(n = "BEZ", colnames = c(
      "dHistId",
      "cId",
      "dId",
      "dLongName",
      "dShortName",
      "dEntryMode",
      "dAdmissionNumber",
      "dAdmissionMode",
      "dAdmissionDate",
      "dAbolitionNumber",
      "dAbolitionMode",
      "dAbolitionDate",
      "dDateOfChange"
    )),
    municipality = list(n = "GDE", colnames = c(
      "mHistId",
      "dHistId",
      "cAbbreviation",
      "mId",
      "mLongName",
      "mShortName",
      "mEntryMode",
      "mStatus",
      "mAdmissionNumber",
      "mAdmissionMode",
      "mAdmissionDate",
      "mAbolitionNumber",
      "mAbolitionMode",
      "mAbolitionDate",
      "mDateOfChange"
    ))
  )
  codes <- c(
    `11` = "Municipality",
    `12` = "Area not allocated to mun.",
    `13` = "Cantonal part of lake",
    `15` = "District",
    `16` = "Canton",
    `17` = "Area not allocated to distr.",
    `20` = "First-time registration",
    `21` = "Creation",
    `22` = "Change of name (d)",
    `23` = "Change of name (m)",
    `24` = "Reassignment to other d/c",
    `26` = "Change of area",
    `27` = "Formal renumbering",
    `29` = "Abolition",
    `30` = "Mutation canceled"
  )

  out <- lapply(X = ft, FUN = function(t) {
    logging::logdebug("Parsing data set: %s", t$n)

    fname <- grep(paste0("_", t$n, "(?:_.*)?[.]txt"), file_list$Name, value = TRUE)
    fpath <- file.path(unzip_dir_name, fname)
    dat <- read.table(
      fpath,
      sep = "\t", quote = "", col.names = t$colnames,
      fileEncoding = "ISO8859-15", stringsAsFactors = FALSE
    )

    date.names <- grep("Date", names(dat), value = TRUE)
    logging::logdebug("Date names: %s", date.names)
    for (n in date.names) dat[, n] <- as.Date(dat[, n], format = "%d.%m.%Y")

    mode.names <- grep("Mode", names(dat), value = TRUE)
    logging::logdebug("Mode names: %s", mode.names)
    for (n in mode.names) dat[, n] <- as.character(factor(dat[, n], levels = names(codes), labels = codes))

    integer.names <- grep("Id|Number", names(dat), value = TRUE)
    logging::logdebug("Integer names: %s", integer.names)
    for (n in integer.names) dat[, n] <- as.integer(dat[, n])

    character.names <- names(dat)[vapply(dat, is.character, logical(1))]
    for (n in character.names) Encoding(dat[, n]) <- "UTF-8"

    # Last column is compulsory
    stopifnot(!is.na(dat[, tail(t$colnames, 1)]))

    tibble::as_tibble(dat)
  })

  out$metadata <- tibble::tibble(
    key = "publication_date",
    value = as.character(as.Date(pub_date, format = "%d.%m.%Y"))
  )

  out
}

# Overwrite the mutation package-data
overwrite_data <- function() {
  data <- swc_read_data()

  readr::write_csv(data$canton, new_csv_file("mut/canton"))
  readr::write_csv(data$district, new_csv_file("mut/district_mutations"))
  readr::write_csv(data$municipality, new_csv_file("mut/municipality_mutations"))
  readr::write_csv(data$metadata, new_csv_file("mut/metadata"))

  write_all_mapping_tables()
}

# check if new data is identical to existing data
check_data <- function() {
  data <- swc_read_data()

  changed <-
    !identical(data$canton, swc_get_cantons()) ||
      !identical(data$district, swc_get_district_mutations()) ||
      !identical(data$municipality, swc_get_municipality_mutations())

  changed
}

daff_canton <- function() {
  stopifnot(requireNamespace("daff", quietly = TRUE))

  data <- swc_read_data()

  daff::render_diff(daff::diff_data(swc_get_cantons(), data$canton))
}

daff_district <- function() {
  stopifnot(requireNamespace("daff", quietly = TRUE))

  data <- swc_read_data()

  daff::render_diff(daff::diff_data(swc_get_district_mutations(), data$district))
}

daff_municipality_mutations <- function() {
  stopifnot(requireNamespace("daff", quietly = TRUE))

  data <- swc_read_data()

  daff::render_diff(daff::diff_data(swc_get_municipality_mutations(), data$municipality))
}



#' download latest municipality inventory
#'
#' @export
download_mun_inventory <- function() {
  mun_inventory_url <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/6986904/master"

  zip_file_name <- tempfile(fileext = ".xlsx")
  logging::logdebug(zip_file_name)

  on.exit(unlink(zip_file_name), add = TRUE)

  download.file(mun_inventory_url, zip_file_name, quiet = TRUE, mode = "wb")

  data <- readxl::read_excel(zip_file_name, sheet = 2)

  names(data) <- tolower(names(data))

  data
}

read_mun_csv <- function(file) {
  target_year <- sub(".csv", "", basename(file))
  read.csv(file) %>%
    mutate(target_year = target_year)
}

read_all_data <- function() {
  csv_dir <- system.file("csv/flat", package = "munch")
  all_files <- list.files(csv_dir)
  file_paths <- file.path(csv_dir, all_files)

  file_paths %>%
    purrr::map_df(~ read_mun_csv(.)) %>%
    as_tibble() %>%
    mutate(target_year = as.integer(target_year))
}
