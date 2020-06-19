#' @export
#' @keywords internal
#' @rdname deprecated
swcGetMapping <- function(swc = NULL, ids.from, ids.to) {
  lifecycle::deprecate_soft("0.0.8", "swissCommunes::swcGetMapping()",
                            "swissCommunes::swc_get_mapping()")
  swc_get_mapping(ids.from = ids.from, ids.to = ids.to)
}


#' @export
#' @keywords internal
#' @rdname deprecated
swcGetMutations <- function (swc = NULL) {
  lifecycle::deprecate_soft("0.0.8", "SwissCommunes::swcGetMutations()",
                            "SwissCommunes::swc_get_mutations()")
  as.data.frame(swc_get_mutations())
}
