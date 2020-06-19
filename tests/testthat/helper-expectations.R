# borrowed from {dm}
expect_swc_error <- function(expr, class, regexp = NULL) {
  expect_error(expr, regexp = regexp, class = swc_error(class))
}
