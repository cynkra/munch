#' Import historic commune data from the web
#'
#' This functon returns historic commune data from the \code{SwissHistMunData}
#' dataset.
#'
#' @return A named list with the components \code{canton}, \code{district} and
#'   \code{municipality}, each component contains a data frame.
#'
#' @param force If \code{TRUE}, reload data from the web.
#'
#' @export
swcGetData <- function(force = FALSE) {
  list(
    canton = SwissHistMunData::cantons,
    district = SwissHistMunData::district_mutations,
    municipality = SwissHistMunData::municipality_mutations
  )
}
