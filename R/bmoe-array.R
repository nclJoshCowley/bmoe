#' MCMC Array Type
#'
#' MCMC array structure, `c(n_iters, n_chains, dim(.)[1], dim(.)[2], ...)`,
#'   used in this package.
#'
#' @note For general MCMC array use, consider packages `posterior` and `mcmcr`
#'   or the original object types in `rjags` and `coda`.
#'
#' @param x object. To be coerced to different format or name used in generic.
#' @param varname character. Variable name of the stored quantity.
#' @param fun function. Function to summarise when printing.
#' @param ... Passed on for S3 methods; ignored in `bmoe_array`.
#'
#' @export
bmoe_array <- function(x, ...) {
  UseMethod("bmoe_array")
}


#' @rdname bmoe_array
#' @export
bmoe_array.array <- function(x, ..., varname) {
  names(dim(x))[1:2] <- c("iteration", "chain")

  class(x) <- "bmoe_array"
  attr(x, "varname") <- varname

  return(x)
}


#' @rdname bmoe_array
#' @export
bmoe_array.mcarray <- function(x, ...) {
  which_mcmc_dims <- match(c("iteration", "chain"), names(dim(x)))
  which_par_dims <- setdiff(seq_along(dim(x)), which_mcmc_dims)

  out <- aperm(x, c(which_mcmc_dims, which_par_dims))
  class(out) <- "bmoe_array"
  attr(out, "varname") <- attr(x, "varname")

  return(out)
}


#' @rdname bmoe_array
#' @export
print.bmoe_array <- function(x, fun = mean, ...) {
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


#' @rdname bmoe_array
#'
#' @param .dimnames character.
#'   Default behaviour only adds `.term <chr>`; supplying dimension names
#'   adds a column of indices for each dimension.
#'
#' @export
tidy.bmoe_array <- function(x, ..., .dimnames = NULL) {
  varname <- attr(x, "varname") %||% "par"

  names(dim(x)) <- gsub("^iteration$", ".iter", names(dim(x)))
  names(dim(x)) <- gsub("^chain$", ".chain", names(dim(x)))

  if (!is.null(.dimnames)) {
    stopifnot("Bad dimension names" = length(.dimnames) == length(dim(x)) - 2)
    names(dim(x)) <- c(".iter", ".chain", .dimnames)
  }

  enframed_array <-
    tibble::as_tibble(do.call(expand.grid, lapply(dim(x), seq_len))) |>
    dplyr::mutate(.value = c(.env$x))

  out <-
    dplyr::mutate(
      enframed_array,
      .term = sprintf(
        "%s[%s]",
        varname,
        unite_and_extract(dplyr::pick(-c(".iter", ".chain", ".value")))
      ),
      .after = ".chain"
    )

  if (!is.null(.dimnames)) return(out)

  dplyr::select(out, dplyr::all_of(c(".iter", ".chain", ".term", ".value")))
}


#' Unite Columns into String
#'
#' @param data data frame. Contains columns to be joined.
#' @param sep character. Passed to `paste`.
#'
#' @returns character vector, `paste(df[, 1], ..., df[, N], sep = sep)`.
#'
#' @seealso `tidyr::unite()`.
#'
#' @keywords internal
unite_and_extract <- function(data, sep = ",") {
  do.call(paste, c(sep = sep, data))
}
