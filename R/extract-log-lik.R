#' Extract Log Likelihood from Model Fit
#'
#' Offline calculations of log likelihood(s) from a `bmoe` model fit.
#'
#' @inheritParams bmoe-package
#'
#' @seealso The `loo` package for using these quantities in model comparison.
#'
#' @name bmoe-log-lik
NULL


#' @rdname bmoe-log-lik
#'
#' @returns * `extract_log_lik()`
#'     * Scalar log likelihood for each MCMC iteration.
#'     * `dim(.) = c(n_iters, n_chains, 1)`.
#'
#' @export
extract_log_lik <- function(object, new_data) {
  pw_log_lik <- extract_pointwise_log_lik(object, new_data)

  bmoe_array(
    array(
      apply(pw_log_lik, c(1:2), sum),
      dim = c(dim(pw_log_lik)[c("iteration", "chain")], 1)
    ),
    varname = "log_lik"
  )
}


#' @rdname bmoe-log-lik
#'
#' @returns * `extract_pointwise_log_lik()`
#'     * Pointwise likelihood is sum of each component contribution,
#'       weighted by the allocation probabilities.
#'     * `dim(.) = c(n_iters, n_chains, n_s_new, n_y, n_k)`.
#'
#' @export
extract_pointwise_log_lik <- function(object, new_data) {
  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)

  n_y <- get_dims_from_bmoe_fit(object, "n_y")
  n_k <- get_dims_from_bmoe_fit(object, "n_k")

  new_y <- extract_y(object, new_data)

  y_means <- extract_y_posterior_mean(object, new_data)
  y_sds <- extract_y_posterior_sd(object)
  probs <- extract_allocation_probs(object, new_data)

  pw_lik <-
    pmap_bmoe_array(
      .l = list(.mn = y_means, .sd = y_sds, .probs = probs),
      .f = function(.mn, .sd, .probs) {
        # Joint likelihood is product of univariate normal "densities"
        joint_y_likelihood <- matrix(NA, nrow = n_s_new, ncol = n_k)

        for (ki in seq_len(n_k)) {
          joint_y_likelihood[, ki] <-
            Reduce(`*`, lapply(seq_len(n_y), function(yi) {
              d_lcens_norm(
                new_y[[yi]], .mn[, yi, ki], .sd[yi, ki], log = FALSE
              )
            }))
        }

        rowSums(.probs * joint_y_likelihood)
      },
      varname = "pointwise_likelihood"
    )


  if (min(pw_lik) <= 0) {
    stop("Pointwise likelihood evaluated to 0, unable to 'log'")
  }

  if (anyNA(pw_lik)) {
    stop("Pointwise likelihood contains NA values")
  }

  if (any(is.infinite(pw_lik))) {
    stop("Pointwise likelihood contains non-finite values")
  }

  return(structure(log(pw_lik), varname = "pointwise_log_lik"))
}


#' @rdname bmoe-log-lik
#'
#' @returns * `extract_componentwise_pointwise_log_lik()`
#'     * Evaluated at each component (expert) and each observation in `y`.
#'     * `dim(.) = c(n_iters, n_chains, n_s_new, n_y, n_k)`.
#'
#' @export
extract_componentwise_pointwise_log_lik <- function(object, new_data) {
  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)

  n_k <- get_dims_from_bmoe_fit(object, "n_k")
  n_y <- get_dims_from_bmoe_fit(object, "n_y")

  new_y <- extract_y(object, new_data)

  y_means <- extract_y_posterior_mean(object, new_data)
  y_sds <- extract_y_posterior_sd(object)

  # Interim variable used in subsequent loop
  cur_log_lik <- array(NA, dim = c(n_s_new, n_y, n_k))

  pmap_bmoe_array(
    .l = list(.mn = y_means, .sd = y_sds),
    .f = function(.mn, .sd) {
      for (yi in seq_len(n_y)) {
        for (ki in seq_len(n_k)) {
          cur_log_lik[, yi, ki] <-
            d_lcens_norm(new_y[[yi]], .mn[, yi, ki], .sd[yi, ki], log = TRUE)
        }
      }
      return(cur_log_lik)
    },
    varname = "componentwise_pointwise_log_lik"
  )
}


#' Likelihood of Left-Censored Normally Distributed Vector
#'
#' @param x object. Expected to be left-censored `Surv`.
#' @param mean,sd,log Passed to [`stats::dnorm`] and [`stats::pnorm`].
#'
#' @note Passing non-`Surv` objects lead to `stats::dnorm(...)`, not an error.
#'
#' @keywords internal
d_lcens_norm <- function(x, mean, sd, log = FALSE) {
  if (!inherits(x, "Surv")) return(stats::dnorm(x, mean, sd))

  stopifnot("Expected left-censored data" = attr(x, "type") == "left")

  ifelse(
    as.logical(x[, "status"]),
    stats::dnorm(x[, "time"], mean, sd, log = log),
    stats::pnorm(x[, "time"], mean, sd, log = log)
  )
}
