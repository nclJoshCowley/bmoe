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

  y_posterior_params <-
    calculate_posterior_params(object, new_data, separate = TRUE)

  mfs <- lapply(object$formula$regr, stats::model.frame, data = cur_data)

  y_list <- lapply(mfs, stats::model.response)

  Map(
    function(y, nm, mean, sd) {
      calc_log_lik <- function(...) log(d_lcens_norm(y, ...))
      pmap_bmoe_array(list(mean, sd), calc_log_lik, varname = nm)
    },
    y = y_list,
    nm = lapply(names(y_list), function(.nm) sprintf("log_lik_%s", .nm)),
    mean = y_posterior_params$y_means,
    sd = y_posterior_params$y_sds
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




