swc_get_cantons <- function() {
  load(rda_file("cantons"))
  cantons
}

rda_file <- function(name) {
  system.file("rda", paste0(name, ".rda"), package = "SwissCommunes")
}
