#' @export
#' @keywords internal
#' @rdname deprecated
swcGetMapping <- function(swc = NULL, ids.from, ids.to) {
  # FIXME: Remove
  lifecycle::deprecate_soft(
    "0.0.8", "munch::swcGetMapping()",
    "munch::swc_get_mapping()"
  )
  swc_get_mapping(ids_from = ids.from, ids_to = ids.to)
}


#' @export
#' @keywords internal
#' @rdname deprecated
swcGetMutations <- function(swc = NULL) {
  # FIXME: Remove
  lifecycle::deprecate_soft(
    "0.0.8", "munch::swcGetMutations()",
    "munch::swc_get_mutations()"
  )
  as.data.frame(swc_get_mutations())
}
