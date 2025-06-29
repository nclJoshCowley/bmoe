#' Mixture of Experts Implementation
#'
#' Implementation of Mixture of Experts models.
#'
#' @param y_list list. Response data per element; length is 1 when univariate.
#' @param x_regr,x_wt matrix. Regression and weighting matrices.
#' @inheritParams bmoe
#'
#' @name bmoe_yx
NULL


#' @rdname bmoe_yx
#' @keywords internal
bmoe_yx <- function(y_list, x_regr, x_wt, prior, jags_n, inits = NULL) {
  stopifnot(setequal(
    names(prior),
    c("k", "regr_prec", "wt_prec", "prec_shape", "prec_rate")
  ))

  any_Surv <- any(vapply(y_list, inherits, what = "Surv", logical(1)))

  if (any_Surv) {
    jags_filename <- "bmoe-cens.jags"
    y_data <- transpose_Surv_list(y_list)

  } else {
    jags_filename <- "bmoe.jags"
    y_data <- list(y = do.call(cbind, y_list))
  }

  jags_file <-
    system.file("model", jags_filename, package = "bmoe", mustWork = TRUE)

  jags_data <-
    rlang::list2(
      !!!y_data,
      x_regr = x_regr,
      x_wt = x_wt,
      n_s = nrow(x_regr),
      n_y = length(y_list),
      p_regr = NCOL(x_regr),
      p_wt = NCOL(x_wt)
    )

  message("\n", "Adaptation stage ...")
  jags_model <-
    rjags::jags.model(
      file = jags_file,
      data = c(jags_data, prior),
      inits = inits,
      n.adapt = jags_n$n.adapt,
      n.chains = jags_n$n.chains
    )

  initial_values <- jags_model$state()

  if (jags_n$n.update > 0) {
    message("\n", "Update stage ...")
    stats::update(jags_model, n.iter = jags_n$n.update)
  }

  message("\n", "Sampling stage ...")

  output <- rjags::jags.samples(
    model = jags_model,
    variable.names = c("regr", "wt", "z", "prec"),
    n.iter = jags_n$n.iter,
    thin = jags_n$n.thin
  )

  output <- lapply(output, bmoe_array)

  structure(
    list(
      data = c(jags_data, prior),
      model = jags_model,
      output = output,
      jags_n = jags_n,
      inits = initial_values
    ),
    class = "bmoe_fit"
  )
}


#' Splits a list of `Surv` objects into JAGS data
#'
#' Convert list of `Surv` objects to two matrices of identical dimension.
#'
#' @param y list. Elements not inheriting `Surv` assumed to be uncensored data.
#'
#' @returns List with `y` matrix and `is_nd` matrix.
#' @keywords internal
transpose_Surv_list <- function(y) {
  value_list <-
    lapply(y, function(.y) {
      if (inherits(.y, "Surv")) .y[, "time"] else .y
    })

  is_nd_list <-
    lapply(y, function(.y) {
      if (inherits(.y, "Surv")) !(.y[, "status"]) else FALSE
    })

  list(y = do.call(cbind, value_list), is_nd = do.call(cbind, is_nd_list))
}
