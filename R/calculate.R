#' Generated Quantities from Model Fit
#'
#' Offline calculations can
#'
#' @inheritParams bmoe-package
#' @param z [`bmoe_array`]. Component allocation samples (latent variable).
#' @param separate logical. Output is split by response variable when `TRUE`.
#'
#' @name bmoe-calculate
NULL


#' Retrieve or Simulate New Component Allocation Samples
#'
#' @rdname bmoe-calculate
#' @export
calculate_component_samples <- function(object, new_data) {
  if (is.null(new_data)) return(object$output$z)

  if (object$prior$k == 1) {
    expected_dims <-
      c(dim(object$output$z)[c("iteration", "chain")], nrow(new_data))

    return(bmoe::bmoe_array(array(1, dim = expected_dims), varname = "z"))
  }

  message("Drawing new allocation samples from relevant distribution")

  wt <- object$output$wt
  x_wt <- stats::model.matrix(object$formula$wt, data = new_data)

  all_labels <- seq_len(dim(wt)[4])

  probs <- pmap_bmoe_array(list(wt), function(.wt) softmax(x_wt %*% .wt))

  pmap_bmoe_array(
    list(probs = probs),
    function(probs) {
      apply(probs, 1, function(.p) sample(all_labels, size = 1, prob = .p))
    },
    varname = "z"
  )
}


#' Calculate Model Fit's Posterior Mean(s)
#'
#' @rdname bmoe-calculate
#' @export
calculate_posterior_y_mean <- function(object, new_data, z, separate = FALSE) {
  regr <- object$output$regr
  x_regr <- calculate_x_regr(object, new_data)

  stopifnot(dim(z)[3] == nrow(x_regr))

  out <-
    pmap_bmoe_array(
      list(.z = z, .regr = regr),
      c_posterior_means,
      x_regr = x_regr,
      varname = "y_mean"
    )

  if (isFALSE(separate)) return(out)

  varnames <- sprintf("y_mean_%s", get_names_from_bmoe_fit(object)$y)
  Map(bmoe_array, asplit(out, 4), varname = varnames)
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
#' @rdname bmoe-calculate
#' @export
calculate_posterior_y_sd <- function(object, z, separate = FALSE) {
  out <-
    pmap_bmoe_array(
      list(.z = z, .prec = object$output$prec),
      c_posterior_sds,
      varname = "y_sd"
    )

  if (isFALSE(separate)) return(out)

  varnames <- sprintf("y_sd_%s", get_names_from_bmoe_fit(object)$y)
  Map(bmoe_array, asplit(out, 4), varname = varnames)
}


#' @keywords internal
#' @noRd
c_posterior_sds <- function(.z, .prec) {
  prec_per_obs_foreach_y <- lapply(asplit(.prec, 1), `[`, .z)
  out <- do.call(cbind, prec_per_obs_foreach_y)
  return(1 / sqrt(out))
}


#' Retrieve Regression Design Matrix
#'
#' Helper to access `x_regr` and verify a common RHS to regression formulas.
#'
#' @rdname bmoe-calculate
#' @export
calculate_x_regr <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object$data

  mfs <- lapply(object$formula$regr, stats::model.frame, data = new_data)

  x_regr_list <- unique(lapply(mfs, stats::model.matrix, data = new_data))

  if (length(x_regr_list) > 1) stop("Regression formulas have different RHS")

  return(x_regr_list[[1]])
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
