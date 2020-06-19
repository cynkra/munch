
# error class generator (taken from {dm})---------------------------------------------------

swc_error <- function(x) {
  paste0("swc_error_", x)
}

swc_error_full <- function(x) {
  c(swc_error(x), "swc_error")
}


# error functions ---------------------------------------------------------

abort_only_int_num <- function(arg, ...) {
  params <- list(...)
  classes <- map_chr(params, class)
  wrong_classes <- classes[!classes %in% c("integer", "numeric")]
  abort(error_only_int_num(arg, wrong_classes), class = swc_error_full("only_int_num"))
}

error_only_int_num <- function(arg, wrong_classes) {
  glue::glue("Values for {tick(arg)} can only be of type `integer` or `numeric`, not {commas(tick(wrong_classes))}.")
}

abort_mind_direction_of_time <- function() {
  abort("'date_from' needs to be before 'date_to'.", class = swc_error_full("mind_direction_of_time"))
}

abort_only_date <- function(param, wrong_class) {
  abort(error_only_date(param, wrong_class), class = swc_error_full("only_date"))
}

error_only_date <- function(param, wrong_class) {
  glue::glue("Parameter {tick(param)} can only be of type `Date`, but is of type {tick(wrong_class)}.")
}

abort_time_grid_fail <- function(time_grid) {
  abort(error_time_grid_fail(time_grid), class = swc_error_full("time_grid_fail"))
}

error_time_grid_fail <- function(time_grid) {
  glue::glue("Value {tick(time_grid)} supplied to parameter 'time_grid', options are: 'none', 'monthly', 'quarterly', 'yearly'.")
}
