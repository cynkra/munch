#' Download municipality mutation data
#'
#' @export
swc_read_data <- function() {
  record_hist_url <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/11467407/master"
  zip_file_name <- tempfile(fileext = ".zip")
  logging::logdebug(zip_file_name)
  on.exit(unlink(zip_file_name), add = TRUE)

  download.file(record_hist_url, zip_file_name, quiet = TRUE)

  unzip_dir_name <- tempfile()
  logging::logdebug(unzip_dir_name)
  on.exit(unlink(unzip_dir_name, recursive = TRUE), add = TRUE)

  file.list <- unzip(zip_file_name, list = TRUE)
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

  l <- lapply(X = ft, FUN = function(t) {
    logging::logdebug("Parsing data set: %s", t$n)

    fname <- grep(paste0("_", t$n, "(?:_.*)?[.]txt"), file.list$Name, value = TRUE)
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
    for (n in mode.names) dat[, n] <- factor(dat[, n], levels = names(codes), labels = codes)

    integer.names <- grep("Id|Number", names(dat), value = TRUE)
    logging::logdebug("Integer names: %s", integer.names)
    for (n in integer.names) dat[, n] <- as.integer(dat[, n])

    character.names <- names(dat)[vapply(dat, is.character, logical(1))]
    for (n in character.names) Encoding(dat[, n]) <- "UTF-8"

    # Last column is compulsory
    stopifnot(!is.na(dat[, tail(t$colnames, 1)]))

    tibble::as_tibble(dat)
  })

  l
}

#' Overwrite the mutation package-data
#'
#' @export
overwrite_data <- function() {
  data <- swc_read_data()

  cantons <- data$canton
  district_mutations <- data$district
  municipality_mutations <- data$municipality

  usethis::use_data(cantons, district_mutations, municipality_mutations, overwrite = TRUE)
}


#' check if new data is identical to old data
#'
#' @export
check_data <- function() {
  data <- swc_read_data()

  unchanged <-
    identical(data$canton, swc_get_cantons()) &&
      identical(data$district, swc_get_district_mutations()) &&
      identical(data$municipality, swc_get_municipality_mutations())

  !unchanged
}


#' check if there were no changes in the old data
#'
#' @export
check_past_changes <- function() {
  data <- swc_read_data()

  past_canton <- dplyr::inner_join(data$canton, swc_get_cantons())

  past_district <- dplyr::inner_join(data$district, swc_get_district_mutations())

  past_municipality <- dplyr::inner_join(
    data$municipality,
    swc_get_municipality_mutations()
  )

  unchanged <-
    nrow(past_canton) == nrow(swc_get_cantons()) &&
      nrow(past_district) == nrow(swc_get_district_mutations()) &&
      nrow(past_municipality) == nrow(swc_get_municipality_mutations())

  unchanged
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
