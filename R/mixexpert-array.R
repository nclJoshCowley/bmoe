#' MCMC Array Type
#'
#' MCMC array structure, `c(n_iters, n_chains, dim(.)[1], dim(.)[2], ...)`,
#'   used in this package
#'
#' @note Considered and decided against packages `posterior`, `mcmcr`, `rjags`.
#'   There seems to be no standardised 'best' method to store MCMC output.
#'
#' @param x object. To be coerced to different format.
#'
#' @export
mixexpert_array <- function(x) {
  UseMethod("mixexpert_array")
}


#' @rdname mixexpert_array
#' @export
mixexpert_array.mcarray <- function(x) {
  which_mcmc_dims <- match(c("iteration", "chain"), names(dim(x)))
  which_par_dims <- setdiff(seq_along(dim(x)), which_mcmc_dims)

  out <- aperm(x, c(which_mcmc_dims, which_par_dims))
  class(out) <- "mixexpert_array"
  attr(out, "varname") <- attr(x, "varname")

  return(out)
}


#' @rdname mixexpert_array
#' @export
print.mixexpert_array <- function(x, fun = mean, ...) {
  varname <- attr(x, "varname") %||% "MISSING"

  cat(
    sep = "\n\n",
    sprintf(
      "Output summarised over %s iterations and %s chains:",
      dim(x)["iteration"],
      dim(x)["chain"]
    ),
    sprintf("varname = '%s'", varname)
  )

  which_dims_non_mcmc <-
    setdiff(
      seq_along(dim(x)),
      match(c("iteration", "chain"), names(dim(x)))
    )

  cat("\n")
  print(apply(x, which_dims_non_mcmc, fun), ...)

  invisible(x)
}


#' @rdname mixexpert_array
#' @export
tidy.mixexpert_array <- function(x, ...) {
  varname <- attr(x, "varname") %||% "par"

  names(dim(x)) <- gsub("^iteration$", ".iter", names(dim(x)))
  names(dim(x)) <- gsub("^chain$", ".chain", names(dim(x)))

  out <- tibble::as_tibble(do.call(expand.grid, lapply(dim(x), seq_len)))

  rowwise_toString <- function(.x) do.call(paste, c(sep = ",", .x))

  out |>
    dplyr::mutate(
      tdims = rowwise_toString(dplyr::pick(-c(".iter", ".chain"))),
      .keep = "unused"
    ) |>
    dplyr::mutate(
      .term = sprintf("%s[%s]", varname, .data$tdims),
      .value = c(x),
      .keep = "unused"
    )
}
