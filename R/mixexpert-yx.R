#' Mixture of Experts Implementation
#'
#' Implementation of Mixture of Experts models.
#'
#' @param y_list list. Response data per element; length is 1 when univariate.
#' @param x_regr,x_wt matrix. Regression and weighting matrices.
#' @inheritParams mixexpert
#'
#' @name mixexpert_yx
NULL


#' @rdname mixexpert_yx
#' @keywords internal
mixexpert_yx <- function(y_list, x_regr, x_wt, prior, jags_n, inits) {
  stopifnot(setequal(
    names(prior),
    c("k", "regr_prec", "wt_prec", "prec_shape", "prec_rate")
  ))


  jags_file <-
    system.file("model", "mixexpert.jags", package = "bmoe", mustWork = TRUE)

  jags_data <-
    list(
      y = do.call(cbind, y_list),
      x_regr = x_regr,
      x_wt = x_wt,
      n_s = nrow(x_regr),
      n_y = length(y_list),
      p_regr = NCOL(x_regr),
      p_wt = NCOL(x_wt)
    )

  out <-
    mcmcrutils::complete_jags_fit(
      file = jags_file,
      data = c(jags_data, prior),
      inits = inits,
      jags_n = jags_n,
      varnames = c("regr", "wt", "z", "prec")
    )

  return(structure(out, class = "mixexpert"))
}
