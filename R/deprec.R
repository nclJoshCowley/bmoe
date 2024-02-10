#' Deprecated Functions
#'
#' These functions should not be used and will be removed in `v1.0.0`.
#'
#' @inheritParams bmoe-package
#' @param z [`bmoe_array`]. Component allocation samples (latent variable).
#' @param separate logical. Output is split by response variable when `TRUE`.
#'
#' @name bmoe-deprec
NULL


#' @rdname bmoe-deprec
#' @export
calculate_pointwise_log_lik <- function(object, new_data) {
  lifecycle::deprecate_warn(
    what = "calculate_pointwise_log_lik()",
    when = "v1.0.0",
    details = "Please use `extract_pointwise_log_lik()` instead"
  )

  cur_data <- if (is.null(new_data)) object$data else new_data

  mfs <- lapply(object$formula$regr, stats::model.frame, data = cur_data)
  y_datalist <- lapply(mfs, stats::model.response)

  z <- calculate_component_samples(object, new_data)

  y_mean <- calculate_posterior_y_mean(object, new_data, z, separate = TRUE)
  y_sd <- calculate_posterior_y_sd(object, z, separate = TRUE)


  Map(
    function(y, nm, mean, sd) {
      pmap_bmoe_array(
        list(mean, sd),
        function(.mean, .sd) d_lcens_norm(y, .mean, .sd, log = TRUE),
        varname = nm
      )
    },
    y = y_datalist,
    nm = lapply(names(y_datalist), function(.nm) sprintf("log_lik_%s", .nm)),
    mean = y_mean,
    sd = y_sd
  )
}


#' @rdname bmoe-deprec
#' @export
calculate_log_lik <- function(object, new_data) {
  lifecycle::deprecate_warn(
    what = "calculate_log_lik()",
    when = "v1.0.0",
    details = "Please use `extract_log_lik()` instead"
  )

  lapply(
    calculate_pointwise_log_lik(object, new_data),
    function(.x) {
      structure(
        apply(.x, 1:2, sum),
        dim = c(dim(.x)[c("iteration", "chain")], 1),
        class = "bmoe_array"
      )
    }
  )
}


#' @rdname bmoe-deprec
#' @export
calculate_component_samples <- function(object, new_data) {
  lifecycle::deprecate_warn(
    what = "calculate_component_samples()",
    when = "v1.0.0",
    details = "Please use `extract_allocation_samples()` instead"
  )

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


#' @rdname bmoe-deprec
#' @export
calculate_x_regr <- function(object, new_data) {
  lifecycle::deprecate_warn(
    what = "calculate_x_regr()",
    when = "v1.0.0",
    details = "Please use `extract_x_regr()` instead"
  )

  extract_x_regr(object, new_data)
}


#' @rdname bmoe-deprec
#' @export
calculate_posterior_y_mean <- function(object, new_data, z, separate = FALSE) {
  lifecycle::deprecate_warn(
    what = "calculate_posterior_y_mean()",
    when = "v1.0.0",
    details = "Please use `extract_y_posterior_mean()` instead"
  )


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


#' @rdname bmoe-deprec
#' @export
calculate_posterior_y_sd <- function(object, z, separate = FALSE) {
  lifecycle::deprecate_warn(
    what = "calculate_posterior_y_sd()",
    when = "v1.0.0",
    details = "Please use `extract_y_posterior_sd()` instead"
  )

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
c_posterior_means <- function(.z, .regr, x_regr) {
  n_y <- ncol(.regr)

  out <- matrix(NA, nrow = length(.z), ncol = n_y)
  for (yi in seq_len(n_y)) {
    out[, yi] <- slice_by_component_values(x_regr %*% .regr[, yi, ], .z)
  }

  return(out)
}


#' @keywords internal
#' @noRd
c_posterior_sds <- function(.z, .prec) {
  prec_per_obs_foreach_y <- lapply(asplit(.prec, 1), `[`, .z)
  out <- do.call(cbind, prec_per_obs_foreach_y)
  return(1 / sqrt(out))
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
