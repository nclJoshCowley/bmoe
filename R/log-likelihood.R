#' Extract Pointwise Log Likelihood
#'
#' Offline computation of log likelihood for each response variable.
#'
#' @inheritParams bmoe-package
#'
#' @seealso The `loo` package for using these quantities in model comparison.
#'
#' @name bmoe-log-lik
#' @export
extract_pointwise_log_lik <- function(object, new_data) {
  cur_data <- if (is.null(new_data)) object$data else new_data

  mfs <- lapply(object$formula$regr, stats::model.frame, data = cur_data)
  y_datalist <- lapply(mfs, stats::model.response)

  z <- calculate_component_samples(object, new_data)

  y_mean <- calculate_posterior_y_mean(object, new_data, z, separate = TRUE)
  y_sd <- calculate_posterior_y_sd(object, z, separate = TRUE)


  Map(
    function(y, nm, mean, sd) {
      calc_log_lik <- function(.mean, .sd) log(d_lcens_norm(y, .mean, .sd))
      pmap_bmoe_array(list(mean, sd), calc_log_lik, varname = nm)
    },
    y = y_datalist,
    nm = lapply(names(y_datalist), function(.nm) sprintf("log_lik_%s", .nm)),
    mean = y_mean,
    sd = y_sd
  )
}


#' @rdname bmoe-log-lik
#' @export
extract_log_lik <- function(object, new_data) {
  lapply(
    extract_pointwise_log_lik(object, new_data),
    function(.x) {
      structure(
        apply(.x, 1:2, sum),
        dim = c(dim(.x)[c("iteration", "chain")], 1),
        class = "bmoe_array"
      )
    }
  )
}


#' Likelihood of Left-Censored Normally Distributed Vector
#'
#' @param x object. Expected to be left-censored `Surv`.
#' @param mean,sd numeric. Passed to `stats::dnorm` or `stats::pnorm`.
#'
#' @note Passing non-`Surv` objects lead to `stats::dnorm(...)`, not an error.
#'
#' @keywords internal
d_lcens_norm <- function(x, mean, sd) {
  if (!inherits(x, "Surv")) return(stats::dnorm(x, mean, sd))

  stopifnot("Expected left-censored data" = attr(x, "type") == "left")

  ifelse(
    as.logical(x[, "status"]),
    stats::dnorm(x[, "time"], mean, sd),
    stats::pnorm(x[, "time"], mean, sd)
  )
}
