#' Get municipality mutation history
#'
#' The history goes back as far as the mutation number becomes 1000
#'   (first time registration)
#'
#' @param munId Municipality Id as integer
#'
#' @examples
#' waedenswil_history <- swc_get_mun_history(293)
#'
#' @export
swc_get_mun_history <- function(munId) {
  mutations <- swc_get_mutations()

  mutations_reduced <- dplyr::select(
    mutations,
    mMutationNumber,
    mMutationId,
    cAbbreviation,
    mHistId.x, mId.x,
    mShortName.x,
    mHistId.y, mId.y,
    mShortName.y,
    mAdmissionMode,
    mMutationDate
  )

  t <- dplyr::filter(mutations_reduced, mId.y == munId)

  t <- dplyr::filter(t, mHistId.y == max(mHistId.y))

  success <- 1

  while (success != 1000) {
    vector <- 1

    t <- purrr::reduce(
      .x = vector,
      ~ add_past(..1, mutations = mutations_reduced),
      .init = t
    )

    success <- as.numeric(max(unique(na.omit(t[1]))))
  }

  t
}



#' Add the past to original data
#'
#' The past is searched by the mHistId.x of the origial file
#'
#' @param x The original data as tibble.
#'
#' @param mutations Mutation file
#'
#' @examples
#' mutations <- swcGetMutations()
#'
#' t <-
#'   dplyr::filter(mutations, mId.y == 293)
#'
#' t_1 <- dplyr::filter(t, mHistId.y == max(mHistId.y))
#'
#' t_past <- add_past(t, mutations)
#'
#' @export
add_past <- function(x, mutations) {
  history <- dplyr::filter(mutations, mHistId.y %in% x$mHistId.x)

  t_added <- dplyr::full_join(history, x, by = c("mHistId.y" = "mHistId.x"))
}
