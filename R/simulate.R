#' Simulate **Mixture of Experts Linear Regression**
#'
#' Simulates from a VISP model and splits into training and test splits.
#'
#' @details
#' * Since `dim(regr) = c(n_x + 1, n_y, n_k)`, the dimension of that input
#'   also asserts `n_x`, `n_y` and `n_k`.
#' * Component membership is simulated per observation, not per response
#'   variable. Hence, `dim(wt) = c(n_x + 1, n_k)`.
#' * The simulated `x` matrix is used for both regression and weighting.
#'
#' @param n_s integer. Number of observations.
#' @param regr array. Regression coefficients.
#' @param wt matrix. Weighting coefficients.
#' @param prec numeric. Precision parameter per component.
#' @param n_loo integer. Number of observations to be split into a test set.
#' @param q_cens numeric \[0, 1\]. Optional artificial left-censoring level.
#'
#' @return List with class `mixexpert_sim`.
#'
#' @export
simulate_mixexpert <- function(n_s, regr, wt, prec, n_loo, q_cens = NULL) {
  stopifnot(length(dim(regr)) == 3)

  n_x <- dim(regr)[1] - 1
  n_y <- dim(regr)[2]
  n_k <- dim(regr)[3]

  stopifnot("Bad dimensions for 'wt'" = dim(wt) == c(n_x + 1, n_k))

  if (length(prec) == 1) prec <- rep(prec, n_k)

  x_data <- simulate_x(n_s, n_x)
  x <- as.matrix(cbind(Intercept = 1, x_data))

  z <- simulate_multilogit(x, wt)

  y <- array(NA, dim = c(n_s, n_y))

  for (i in 1:n_s) {
    for (j in 1:n_y) {
      y[i, j] <-
        stats::rnorm(
          n = 1,
          mean = drop(x[i, ] %*% regr[, j, z[i]]),
          sd = sqrt(1 / prec[z[i]])
        )
    }
  }

  y_data <-
    tibble::as_tibble(y, .name_repair = \(.y) sprintf("y%02i", seq_along(.y)))

  all_data <- dplyr::bind_cols(y_data, x_data)
  is_test <- seq_len(n_s) %in% sample.int(n_s, size = n_loo)

  out <-
    list(
      data = all_data[!is_test, ],
      params = list(
        z = z[!is_test], new_z = z[is_test], regr = regr, wt = wt, prec = prec
      ),
      new_data = all_data[is_test, ]
    )

  if (!is.null(q_cens)) {
    stop("Artificial censoring not implemented yet")
  }

  return(structure(out, class = "mixexpert_sim"))
}


#' @rdname simulate_mixexpert
#'
#' @param ... Passed to `simulate_mixexpert`.
#' @param multiple_y logical. When `TRUE`, example defaults to multiple `y`.
#'
#' @export
example_simulate_mixexpert <- function(..., multiple_y = FALSE) {
  regr <-
    if (multiple_y) {
      array(sample.int(24), dim = c(4, 2, 3))
    } else {
      array(sample.int(12), dim = c(4, 1, 3))
    }

  args <-
    utils::modifyList(val = rlang::list2(...), list(
      n_s = 200,
      regr = regr,
      wt = sweep_wt(array(0.1 * sample.int(12), dim = c(4, 3)), ref = 1),
      prec = c(0.1, 0.5, 2.5),
      n_loo = 20,
      q_cens = NULL
    ))

  do.call(simulate_mixexpert, args)
}


#' Multinomial Logit Simulation
#'
#' @param x_wt matrix (`n` by `p_w`). Design matrix to be used in weighting.
#' @param wt matrix (`p_w` by `k`). Weighting regression matrix.
#'
#' @export
simulate_multilogit <- function(x_wt, wt) {
  probs <- bmoe::softmax(x_wt %*% wt)

  z <- apply(
    probs,
    MARGIN = 1,
    FUN = function(.prob) sample(seq_along(.prob), size = 1, prob = .prob)
  )

  return(z)
}


#' Modify Weighting Matrix Reference Column
#'
#' @param wt matrix. Weighting matrix.
#' @param ref integer. Desired reference column to be set to 0.
#'
#' @keywords internal
sweep_wt <- function(wt, ref = 1) sweep(wt, 1, wt[, ref], `-`)


#' Simulate Design Matrix
#'
#' A design matrix of chosen width is simulated for use in studies.
#'
#' @param n_s integer. Number of observations (rows).
#' @param n_x integer. Number of explanatory variables (columns).
#' @param .scale logical. When `TRUE`, output is scaled before returned.
#'
#' @keywords internal
simulate_x <- function(n_s, n_x, .scale = TRUE) {
  out <- MASS::mvrnorm(n = n_s, mu = rep(0, n_x), Sigma = diag(1, nrow = n_x))

  if (isTRUE(.scale)) {
    out <- scale(out)
    attr(out, "scaled:center") <- NULL
    attr(out, "scaled:scale") <- NULL
  }

  tibble::as_tibble(out, .name_repair = \(.x) sprintf("x%02i", seq_along(.x)))
}
