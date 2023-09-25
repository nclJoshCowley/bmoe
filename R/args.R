#' Bayesian MoE (`bmoe`) Arguments
#'
#' Helper functions to assist in supplying arguments to [`bmoe()`].
#'
#' @param ... Overwrite default values.
#'
#' @name bmoe-args
NULL


#' @rdname bmoe-args
#'
#' @param k integer. Assumed number of components.
#'
#' @section Prior:
#' Hyper-parameters that must be passed to the `prior` argument are in bold.
#' - `k`, number of components, assumed known.
#' - `regr` is element-wise IID Normal with 0 mean and **regr_prec** precision.
#' - `wt` is element-wise IID Normal with 0 mean and **wt_prec** precision.
#' - `prec` is element-wise IID Gamma with **prec_shape** and **prec_rate**.
#'
#' @export
bmoe_prior <- function(k, ...) {
  defaults <-
    list(
      k = k, regr_prec = 0.1, wt_prec = 1, prec_shape = 2, prec_rate = 1
    )

  out <- utils::modifyList(defaults, purrr::list_flatten(rlang::list2(...)))

  stopifnot("Invalid prior" = setequal(names(defaults), names(out)))

  return(out)
}


#' @rdname bmoe-args
#'
#' @section JAGS Controls:
#' * `n.adapt` controls number of discarded samples in adaptation stage.
#' * `n.update` controls number of discarded samples in warm-up stage.
#' * `n.iter` controls how many samples are saved.
#' * `n.thin` controls thinning, where only every \eqn{n^th} sample is kept.
#' * `n.chains` controls number of chains.
#'
#' @export
bmoe_jags_n <- function(...) {
  defaults <-
    list(
      n.adapt = 2e3, n.update = 2e3, n.iter = 1e4, n.thin = 1, n.chains = 2
    )

  out <- utils::modifyList(defaults, purrr::list_flatten(rlang::list2(...)))

  invalid_names <- setdiff(names(out), names(defaults))
  if (length(invalid_names) > 0) {
    stop("Unrecognised JAGS control(s): ", toString(invalid_names))
  }

  return(out)
}
