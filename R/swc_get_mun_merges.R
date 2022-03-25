#' Get municipality merges for a certain year and cantona
#'
#' @param year The year where the merges took place
#'
#' @param canton The abbreviation as characger
#'   of a canton from which the merges should be returned.
#'   If NULL, all merges are returned
#'
#' @examples
#' zuerich_merges <- swc_get_mun_merges(year = 2019, canton = "ZH")
#'
#' @export
swc_get_mun_merges <- function(year = NULL, canton = NULL) {
  if (is.null(canton)) {
    mutations <- swc_get_mutations()
  } else {
    mutations <- swc_get_mutations(canton = canton)
  }

  mun.mut.year <- subset(
    mutations,
    mMutationDate >= as.Date(paste0(year, "-01-01")) &
      mMutationDate < as.Date(paste0(year, "-12-31"))
  )

  mun.mut.all <- merge(
    mun.mut.year,
    mun.mut.year,
    by.x = "mHistId.y", by.y = "mHistId.x", all = T, incomparables = NA
  )

  mun.mut <- subset(mun.mut.all, !is.na(get("mMutationNumber.x")))

  mun.mut.red <- dplyr::filter(
    mun.mut,
    !(mMutationNumber.x %in% unique(na.omit(mun.mut$mMutationNumber.y)))
  )

  mun.mut.red$rename <- dplyr::if_else(grepl(c("name"), mun.mut.red$mAdmissionMode.x),
    mun.mut.red$mShortName.y.x,
    mun.mut.red$mShortName.y.y
  )

  mun.mut.red$rename_date <- dplyr::if_else(grepl(c("name"), mun.mut.red$mAdmissionMode.x),
    mun.mut.red$mMutationDate.x,
    mun.mut.red$mMutationDate.y
  )


  mun.mut.red$mShortName.y.x <- dplyr::if_else(grepl(c("name"), mun.mut.red$mAdmissionMode.x),
    NA_character_,
    mun.mut.red$mShortName.y.x
  )

  mun.mut.red$mMutationDate.x <- dplyr::if_else(grepl(c("name"), mun.mut.red$mAdmissionMode.x),
    structure(NA_real_, class = "Date"),
    mun.mut.red$mMutationDate.x
  )


  mun.mut.fus <- dplyr::filter(
    mun.mut.red,
    !grepl(c("Reassignment"), mAdmissionMode.x)
  )

  mun.mut.final <- dplyr::select(mun.mut.fus,
    canton = cAbbreviation.x,
    mId_from = mId.x.x,
    merge_name_from = mShortName.x.x,
    mId_to = mId.y.x,
    merge_name_to = mShortName.y.x,
    merge_date = mMutationDate.x,
    rename,
    rename_date
  )
}
