#' Import historic commune data from the web
#' 
#' This functon returns historic commune.  If necessary,
#' \code{\link{read_historicized_commune_data}} is called.  The data is cached
#' with the help of the \link{R.cache} package.
#' 
#' @return A named list with the components \code{canton}, \code{district} and
#'   \code{municipality}, each component contains a data frame.
#' 
#' @param force If \code{TRUE}, reload data from the web.
#' 
#' @usage get_historicized_commune_data(force = FALSE)
#' 
#' @seealso \code{\link{read_historicized_commune_data}}
#' 
#' @export
#' @importFrom R.cache addMemoization
#' @include read_historicized_commune_data.R
get_historicized_commune_data <- R.cache::addMemoization(read_historicized_commune_data)
