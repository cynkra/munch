#' Import historic commune data from the web
#'
#' This functon returns historic commune data.  If necessary,
#' \code{\link{swcReadData}} is called.  The data are cached
#' with the help of the \link{R.cache} package.  The package version and the
#' current month are used as key; if one of these changes, the data are
#' downloaded again.
#'
#' @return A named list with the components \code{canton}, \code{district} and
#'   \code{municipality}, each component contains a data frame.
#'
#' @param force If \code{TRUE}, reload data from the web.
#'
#' @seealso \code{\link{swcReadData}}
#'
#' @export
#' @importFrom R.cache evalWithMemoization
#' @include swcReadData.R
swcGetData <- function(force=FALSE) {
  key <- list(version = packageVersion("SwissCommunes"),
              month = format(Sys.Date(), "%Y-%m"))
  R.cache::evalWithMemoization(swcReadData(), key=key, force=force)
}
