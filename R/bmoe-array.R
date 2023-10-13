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
  names(dim(x))[is.na(names(dim(x)))] <- ""

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


#' Apply function to each MCMC Draw
#'
#' Maps a function over each MCMC iteration and chain of a [`bmoe_array`].
#'
#' @param .l list of `bmoe_array`. Only MCMC inputs should be placed here.
#' @param .f function. To be applied over each draws.
#' @param ... Deterministic arguments. Same value are passed to `.f` each time.
#' @inheritParams bmoe_array
#'
#' @returns A newly generated MCMC quantity, stored as a `bmoe_array`.
#'
#' @export
pmap_bmoe_array <- function(.l, .f, ..., varname = NULL) {
  stopifnot("MCMC objects should be wrapped in a list" = is.list(.l))

  ni <- unique(vapply(.l, nrow, integer(1)))
  if (length(ni) > 1) stop("Number of iterations mismatch: ", toString(ni))

  nc <- unique(vapply(.l, ncol, integer(1)))
  if (length(nc) > 1) stop("Number of chains mismatch: ", toString(ni))

  # Need to perform calculation once to get an idea of dimensions
  l_proto <- lapply(.l, extract_single_draw, chain = 1, iter = 1)
  out_proto <- do.call(.f, c(l_proto, list(...)))

  out_dims <- dim(out_proto) %||% length(out_proto)

  out <- array(NA, dim = c(ni, nc, out_dims))

  lhs_expr <- quote(out[ii, ic, ])[c(1:4, rep(5, length(out_dims)))]

  for (ii in seq_len(ni)) {
    for (ic in seq_len(nc)) {
      current_l <- lapply(.l, extract_single_draw, chain = ic, iter = ii)
      eval(rlang::expr(
        !!lhs_expr <- do.call(.f, c(current_l, list(...)))
      ))
    }
  }

  return(bmoe_array.array(out, varname = varname))
}


#' Extract Single Draw
#'
#' Extract a single draw for specified iteration and chain.
#'
#' @note Only the MCMC dimensions are [dropped][drop].
#'
#' @param x [`bmoe_array`] object.
#' @param iter,chain integer. MCMC indexes to extract from.
#'
#' @keywords internal
extract_single_draw <- function(x, iter, chain) {
  vardims <- dim(x)[-c(1, 2)]

  missing_args <- rep(list(rlang::missing_arg()), length(vardims))

  out_expr <-
    rlang::call2("[", quote(x), iter, chain, !!!missing_args, drop = FALSE)

  return(structure(eval(out_expr), dim = vardims))
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
