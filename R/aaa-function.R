new_fun_forward <- function(fwd, env = caller_env(), old_fwd_name, new_name = NULL) {
  fwd_sym <- ensym(fwd)
  if (is_null(new_name)) fwd_name <- as_name(fwd_sym) else fwd_name <- new_name

  args <- formals(fwd)
  fwd_args <- set_names(syms(names(args)), names(args))

  body <- expr({
    lifecycle::deprecate_soft(
      "0.0.8",
      !!paste0("swissCommunes::", old_fwd_name, "()"),
      !!paste0("swissCommunes::", fwd_name, "()")
    )
    (!!fwd_sym)(!!!fwd_args)
  })

  new_function(args, body, env)
}
