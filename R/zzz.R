.onLoad <- function(libname, pkgname) {
  requireNamespace("memoise", quietly = TRUE)
  calculate_component_samples <<- memoise::memoise(calculate_component_samples)
}
