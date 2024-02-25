#' Extract Generated Quantities from Model Fit
#'
#' Offline calculations for model artefacts from a `bmoe` model fit.
#'
#' @inheritParams bmoe-package
#'
#' @name bmoe-extract-quantities
#'
#' @seealso [Allocated versions][bmoe-extract-allocated] of these functions.
#'
#' @returns All quantities returned as [`bmoe_array()`] of varying dimension.
#'
#' Note that `n_s_new` denotes the number of rows in `new_data` or the
#'   observed data when `new_data` is `NULL` valued.
NULL


#' @rdname bmoe-extract-quantities
#'
#' @returns * `extract_y_posterior_mean()`
#'     * `dim(.) = c(n_iters, n_chains, n_s_new, n_y, n_k)`.
#'
#' @export
extract_y_posterior_mean <- function(object, new_data) {
  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)

  n_y <- get_dims_from_bmoe_fit(object, "n_y")
  n_k <- get_dims_from_bmoe_fit(object, "n_k")

  x_regr <- extract_x_regr(object, new_data)

  # Interim variable used in subsequent loop
  cur_y_mean <- array(NA, dim = c(n_s_new, n_y, n_k))

  pmap_bmoe_array(
    .l = list(.regr = object$output$regr),
    .f = function(.regr) {
      for (yi in seq_len(n_y)) {
        cur_y_mean[, yi, ] <- x_regr %*% .regr[, yi, ]
      }
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


#' @rdname bmoe-extract-quantities
#'
#' @returns * `extract_allocation_probs()`
#'     * `dim(.) = c(n_iters, n_chains, n_s_new, n_k)`.
#'
#' @export
extract_allocation_probs <- function(object, new_data) {
  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)

  n_k <- get_dims_from_bmoe_fit(object, "n_k")

  # Special case -- Assumed single component implies unital probabilities
  if (n_k == 1) {
    n_iters <- get_dims_from_bmoe_fit(object, "n_iters")
    n_chains <- get_dims_from_bmoe_fit(object, "n_chains")

    return(bmoe::bmoe_array(
      array(1, dim = c(n_iters, n_chains, n_s_new, 1)),
      varname = "allocation_probs"
    ))
  }

  x_wt <- extract_x_wt(object, new_data)

  pmap_bmoe_array(
    .l = list(.wt = object$output$wt),
    .f = function(.wt) bmoe::softmax(x_wt %*% .wt),
    varname = "allocation_probs"
  )
}


#' @rdname bmoe-extract-quantities
#'
#' @returns * `extract_allocation_samples()`
#'     * `dim(.) = c(n_iters, n_chains, n_s_new)`.
#'
#' @export
extract_allocation_samples <- function(object, new_data) {
  # Special case -- Using observed data implies use of saved samples
  if (is.null(new_data)) {
    message("Using previously sampled allocation draws")
    return(structure(object$output$z, varname = "allocation_samples"))
  }

  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)

  n_k <- get_dims_from_bmoe_fit(object, "n_k")

  # Special case -- Assumed single component implies unital samples
  if (n_k == 1) {
    n_iters <- get_dims_from_bmoe_fit(object, "n_iters")
    n_chains <- get_dims_from_bmoe_fit(object, "n_chains")

    return(bmoe::bmoe_array(
      array(1, dim = c(n_iters, n_chains, n_s_new)),
      varname = "allocation_samples"
    ))
  }

  alloc_probs <- extract_allocation_probs(object, new_data)

  sample_alloc <- function(prob) sample(seq_len(n_k), size = 1, prob = prob)

  pmap_bmoe_array(
    .l = list(.alloc_probs = alloc_probs),
    .f = function(.alloc_probs) apply(.alloc_probs, 1, sample_alloc),
    varname = "allocation_samples"
  )
}
