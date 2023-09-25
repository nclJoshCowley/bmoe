#' Extract Pointwise Log Likelihood
#'
#' Offline computation of log likelihood for each response variable.
#'
#' @inheritParams bmoe-package
#' @param data data frame. Used in calculation;
#'   `object$data` yields in-sample result.
#'
#' @seealso The `loo` package for using these quantities in model comparison.
#'
#' @name bmoe-log-lik
#' @export
extract_pointwise_log_lik <- function(object, data) {
  mfs <- lapply(object$formula$regr, stats::model.frame, data = data)
  y_list <- lapply(mfs, stats::model.response)
  x_regr <- unique(lapply(mfs, stats::model.matrix, data = data))

  stopifnot("Common x_regr assumption violated" = length(x_regr) == 1)
  x_regr <- x_regr[[1]]

  y_means <- extract_posterior_expectations(object, x_regr)

  # n_y <- length(y_list)
  n_iters <- dim(object$output$regr)["iteration"]
  n_chains <- dim(object$output$regr)["chain"]
  n_s <- nrow(x_regr)

  out <-
    lapply(rlang::set_names(names(y_list)), function(.y) {
      structure(
        array(NA, dim = c(n_iters, n_chains, n_s)),
        class = "bmoe_array",
        varname = sprintf("%s_log_lik", .y)
      )
    })


  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      for (iy in seq_along(y_list)) {
        cur_y <- y_list[[iy]]
        cur_mean <- y_means[[iy]][ii, ic, ]

        cur_prec <-
          vapply(
            object$output$z[ii, ic, ],
            function(.z) object$output$prec[ii, ic, iy, .z],
            numeric(1)
          )

        out[[iy]][ii, ic, ] <-
          log(d_lcens_norm(cur_y, cur_mean, 1 / sqrt(cur_prec)))
      }
    }
  }

  return(out)
}

#' @rdname bmoe-log-lik
#' @export
extract_log_lik <- function(object, data) {
  lapply(
    extract_pointwise_log_lik(object, data),
    function(.x) {
      structure(
        apply(.x, 1:2, sum),
        dim = c(dim(.x)[c("iteration", "chain")], 1),
        class = "bmoe_array"
      )
    }
  )
}


#' Extract Posterior Expectation(s)
#'
#' Offline calculation of posterior means, denoted `mean` in JAGS file.
#'
#' @details
#'   Note that while `x_regr` and `regr` lead to a mean matrix of `n_s` rows
#'   and `K` columns, we flatten this structure into a `n_s` vector by using
#'   the component allocation samples, `z`.
#'
#' @inheritParams bmoe-package
#' @param x_regr matrix. Design matrix to use in the calculation.
#'
#' @returns List of MCMC arrays denoting the mean vector, one element per
#'   response variable.
#'
#' @export
extract_posterior_expectations <- function(object, x_regr) {
  y_nms <- rlang::set_names(get_names_from_bmoe_fit(object)$y)

  n_iters <- dim(object$output$regr)["iteration"]
  n_chains <- dim(object$output$regr)["chain"]
  n_y <- length(y_nms)
  n_s <- nrow(x_regr)

  out <-
    lapply(y_nms, function(.y) {
      structure(
        array(NA, dim = c(n_iters, n_chains, n_s)),
        class = "bmoe_array",
        varname = sprintf("%s_mean", .y)
      )
    })

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      for (iy in seq_len(n_y)) {
        cur_mean_matrix <- x_regr %*% object$output$regr[ii, ic, , iy, ]
        cur_z <- object$output$z[ii, ic, ]

        out[[iy]][ii, ic, ] <-
          mapply(
            function(i, z) cur_mean_matrix[i, z],
            i = seq_along(cur_z),
            z = cur_z
          )
      }
    }
  }

  return(out)
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
