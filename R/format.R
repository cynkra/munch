# Next two are borrowed from {dm}
MAX_COMMAS <- 6L

commas <- function(x, max_commas = MAX_COMMAS, capped = FALSE) {
  if (is_null(max_commas)) max_commas <- MAX_COMMAS
  if (is_empty(x)) {
    x <- ""
  } else if (length(x) > max_commas) {
    x[[max_commas]] <- paste0(
      cli::symbol$ellipsis, " (",
      if (capped) ">= ",
      length(x),
      " total)"
    )
    length(x) <- max_commas
  }

  glue_collapse(x, sep = ", ")
}

tick <- function(x) {
  if (is_empty(x)) {
    return(character())
  }
  paste0("`", x, "`")
}
