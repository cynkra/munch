#' @export
#' @keywords internal
#' @rdname deprecated
swcGetMapping <- new_fun_forward(swc_get_mapping, old_fwd_name = "swcGetMapping")

#' @export
#' @keywords internal
#' @rdname deprecated
swcGetMutations <- function (swc = NULL) {
  lifecycle::deprecate_soft("0.0.8", "SwissCommunes::swcGetMutations()",
                            "SwissCommunes::swc_get_mutations()")
  as.data.frame(swc_get_mutations())
}
