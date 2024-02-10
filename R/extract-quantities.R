#' Extract Generated Quantities from Model Fit
#'
#' Offline calculations for model artefacts from a `bmoe` model fit.
#'
#' @inheritParams bmoe-package
#'
#' @name bmoe-extract-quantities
#'
#' @returns All quantities returned as [`bmoe_array()`] of varying dimension.
NULL


#' @rdname bmoe-extract-quantities
#'
#' @returns * `extract_y_posterior_mean()`
#'     * `dim(.) = c(n_iters, n_chains, n_s, n_y, n_k)`.
#'
#' @export
extract_y_posterior_mean <- function(object, new_data) {
  n_s <- get_dims_from_bmoe_fit(object, "n_s")
  n_y <- get_dims_from_bmoe_fit(object, "n_y")
  n_k <- get_dims_from_bmoe_fit(object, "n_k")

  x_regr <- extract_x_regr(object, new_data)

  pmap_bmoe_array(
    .l = list(.regr = object$output$regr),
    .f = function(.regr) {
      cur_y_mean <- array(NA, dim = c(n_s, n_y, n_k))
      for (yi in seq_len(n_y)) cur_y_mean[, yi, ] <- x_regr %*% .regr[, yi, ]
      return(cur_y_mean)
    },
    varname = "y_posterior_mean"
  )
}


#' @rdname bmoe-extract-quantities
#'
#' @returns * `extract_y_posterior_sd()`
#'     * `dim(.) = c(n_iters, n_chains, n_y, n_k)`.
#'
#' @export
extract_y_posterior_sd <- function(object) {
  out <- 1 / sqrt(object$output$prec)
  return(structure(out, varname = "y_posterior_sd"))
}
