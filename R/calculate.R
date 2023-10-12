#' Calculate Model Fit's Posterior Mean and Standard Deviation
#'
#' @inheritParams bmoe-package
#' @param separate logical. Splits the output into multiple arrays when `TRUE`.
#'
#' @family Posterior calculation functions
#' @export
calculate_posterior_params <- function(object, new_data, separate = FALSE) {
  use_trained <- is.null(new_data)

  if (use_trained) new_data <- object$data

  x_regr <- get_x_regr_from_bmoe_fit(object, data = new_data)

  if (use_trained) {
    z <- object$output$z
  } else {
    x_wt <- stats::model.matrix(object$formula$wt, data = new_data)
    z <- calculate_component_samples(object$output$wt, x_wt, "z")
  }

  out <-
    list(
      y_means = calculate_posterior_means(z, object$output$regr, x_regr, NULL),
      y_sds = calculate_posterior_sds(z, object$output$prec, NULL)
    )

  if (isFALSE(separate)) return(out)

  out$y_means <- lapply(asplit(out$y_means, 4), bmoe_array, varname = NULL)
  out$y_sds <- lapply(asplit(out$y_sds, 4), bmoe_array, varname = NULL)
  return(out)
}


#' Calculate Model Fit's Posterior Mean(s)
#'
#' @param z [`bmoe_array`]. Component allocation (latent variable) output.
#' @param regr [`bmoe_array`]. Regression coefficient output.
#' @param x_regr matrix. Required design matrix.
#' @inheritParams bmoe_array
#'
#' @rdname calculate_posterior_params
#' @export
calculate_posterior_means <- function(z, regr, x_regr, varname) {
  stopifnot(dim(z)[3] == nrow(x_regr))

  pmap_bmoe_array(
    list(.z = z, .regr = regr),
    c_posterior_means,
    x_regr = x_regr,
    varname = varname
  )
}


#' @keywords internal
#' @noRd
c_posterior_means <- function(.z, .regr, x_regr) {
  n_y <- ncol(.regr)

  out <- matrix(NA, nrow = length(.z), ncol = n_y)
  for (yi in seq_len(n_y)) {
    out[, yi] <- slice_by_component_values(x_regr %*% .regr[, yi, ], .z)
  }

  return(out)
}


#' Calculate Model Fit's Posterior Standard Deviation(s)
#'
#' @param z [`bmoe_array`]. Component allocation (latent variable) output.
#' @param prec [`bmoe_array`]. Precision matrix output.
#' @inheritParams bmoe_array
#'
#' @rdname calculate_posterior_params
#' @export
calculate_posterior_sds <- function(z, prec, varname) {
  pmap_bmoe_array(
    list(.z = z, .prec = prec),
    c_posterior_sds,
    varname = varname
  )
}


#' @keywords internal
#' @noRd
c_posterior_sds <- function(.z, .prec) {
  prec_per_obs_foreach_y <- lapply(asplit(.prec, 1), `[`, .z)
  out <- do.call(cbind, prec_per_obs_foreach_y)
  return(1 / sqrt(out))
}


#' Simulate New Component Allocation Samples
#'
#' @param wt [`bmoe_array`]. Model contribution to probabilities
#' @param x_wt matrix. Data contribution to probabilities
#' @inheritParams bmoe_array
#'
#' @family Posterior calculation functions
#' @export
calculate_component_samples <- function(wt, x_wt, varname) {
  all_labels <- seq_len(dim(wt)[4])

  probs <- pmap_bmoe_array(list(wt), function(.wt) softmax(x_wt %*% .wt))

  pmap_bmoe_array(
    list(probs = probs),
    function(probs) {
      apply(probs, 1, function(.p) sample(all_labels, size = 1, prob = .p))
    },
    varname = varname
  )
}


#' Slice by Component Values
#'
#' Delegates 'per component' slicing to get
#' ```
#' for (i in seq_along(z)) x[i, z[i]]
#' ```
#' unlike `x[i, z[i]]` which returns a matrix.
#'
#' @keywords internal
slice_by_component_values <- function(x, z) {
  mapply(function(.i, .z) x[.i, .z], .i = seq_along(z), .z = z)
}
