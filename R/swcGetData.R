#' Import historic commune data from the web
#' 
#' This functon returns historic commune.  If necessary,
#' \code{\link{swcReadData}} is called.  The data is cached
#' with the help of the \link{R.cache} package.
#' 
#' @return A named list with the components \code{canton}, \code{district} and
#'   \code{municipality}, each component contains a data frame.
#' 
#' @param force If \code{TRUE}, reload data from the web.
#' 
#' @usage swcGetData(force = FALSE)
#' 
#' @seealso \code{\link{swcReadData}}
#' 
#' @export
#' @importFrom R.cache addMemoization
#' @include swcReadData.R
swcGetData <- R.cache::addMemoization(swcReadData)
