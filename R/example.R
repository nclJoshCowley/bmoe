#' @rdname bmoe
#'
#' @inheritParams simulate_bmoe
#'
#' @export
example_bmoe_fit <- function(..., multiple_y = FALSE) {
  bmoe(
    example_simulate_bmoe(..., multiple_y = multiple_y),
    prior = bmoe_prior(k = 3),
    jags_n = list(n.update = 0, n.adapt = 2000, n.iter = 10000)
  )
}


#' @rdname simulate_bmoe
#'
#' @param ... Passed to `simulate_bmoe`.
#' @param multiple_y logical. When `TRUE`, example defaults to multiple `y`.
#'
#' @export
example_simulate_bmoe <- function(..., multiple_y = FALSE) {
  regr <-
    if (multiple_y) {
      array(0.25 * sample.int(24), dim = c(4, 2, 3))
    } else {
      array(0.25 * sample.int(12), dim = c(4, 1, 3))
    }

  args <-
    utils::modifyList(val = rlang::list2(...), list(
      n_s = 200,
      regr = regr,
      wt = sweep_ref_vals(array(0.1 * sample.int(12), dim = c(4, 3)), ref = 1),
      prec = c(0.1, 0.5, 2.5),
      n_loo = 20,
      q_cens = NULL
    ))

  do.call(simulate_bmoe, args)
}


#' @describeIn simulate_bmoe Reproducible example of label switching example.
#' @export
example_label_switching_bmoe <- function() {
  stop("Not implemented. Parameters causing label switching not defined")

  saved_seed <- get0(".Random.seed")
  on.exit(assign(".Random.seed", saved_seed, envir = .GlobalEnv))
  set.seed(40)

  p_regr <- 6
  k <- 3

  regr <- array(sample.int(p_regr * k), dim = c(p_regr, 1, k))
  prec <- t(c(0.5, 0.5, 1))

  wt <- sweep_ref_vals(0.1 * matrix(sample.int(p_regr * k), p_regr, k))
  # wt <- rbind(c(0, 0.2, -0.1), c(0, 0.4, 0.1))

  sim <-
    bmoe::simulate_bmoe(
      n_s = 80,
      regr = regr,
      wt = wt,
      prec = prec,
      n_loo = 0,
      q_cens = NULL
    )

  out <-
    bmoe::bmoe(
      sim,
      prior = list(
        k = 3,
        regr_prec = 0.1,
        wt_prec = 1,
        prec_shape = 1, prec_rate = 1
      ),
      jags_n = list(n.chains = 1, n.update = 0),
      inits = bmoe::bmoe_inits(seed_base = 1)
    )

  ## DEBUG
  message("regr[,,]) = \n"); print(sim$params$regr)
  message("wt[,]) = \n"); print(sim$params$wt)
  message("table(z[,]) = \n"); print(table(sim$params$z))
  render_bmoe_fit(out, "temp/ls")
  ## DEBUG

  return(out)

}
