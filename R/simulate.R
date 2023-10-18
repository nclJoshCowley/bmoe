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
#' * Only `n_s - n_loo` of the total observations will be kept in `$data`.
#'
#' @param n_s integer. Total number of simulated observations.
#' @param regr array. Regression coefficients.
#' @param wt matrix. Weighting coefficients.
#' @param prec numeric. Precision parameter per component.
#' @param n_loo integer. Number of observations to be moved into a test set.
#' @param q_cens numeric \[0, 1\]. Optional artificial left-censoring level.
#'
#'
#' @returns List with class `bmoe_sim`.
#'
#' @export
simulate_bmoe <- function(n_s, regr, wt, prec, n_loo, q_cens = NULL) {
  stopifnot(length(dim(regr)) == 3)

  n_x <- dim(regr)[1] - 1
  n_y <- dim(regr)[2]
  n_k <- dim(regr)[3]

  stopifnot("Bad dimensions for 'wt'" = dim(wt) == c(n_x + 1, n_k))

  if (is.null(dim(prec))) {
    if (length(prec) == 1) prec <- rep(prec, n_k)
    if (length(prec) == n_k) prec <- matrix(prec, n_y, n_k, byrow = TRUE)
  }

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
    requireNamespace("survival", quietly = TRUE)
    out$params$y_uncens <- out$data[colnames(y_data)]

    out$data <-
      dplyr::mutate(out$data, dplyr::across(
        .cols = dplyr::all_of(colnames(y_data)),
        .fns = function(.y) artificial_Surv(.y, q_cens)
      ))
  }

  return(structure(out, class = "bmoe_sim"))
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

#' Sweep Out Reference Values
#'
#' Translates matrix to satisfy corner constraint, say `x[, ref] = 0`.
#'
#' @returns Matrix with elements defined as `out[i, j] = x[i, j] - x[i, ref]`.
#'
#' @param x matrix. Matrix to be translated.
#' @param ref integer. Column index to be used as reference.
#'
#' @export
sweep_ref_vals <- function(x, ref = 1) sweep(x, 1, x[, ref], FUN = `-`)


#' Artificial Censoring
#'
#' Convert numeric vector to a `survival::Surv` object by censoring at
#'   chosen quantiles.
#'
#' @param x vector. Uncensored numerical data.
#' @inheritParams simulate_bmoe
#'
#' @returns Surv object.
#' @keywords internal
artificial_Surv <- function(x, q_cens) {
  detlims <- stats::quantile(x, q_cens)

  cut_points <- cut(x, unique(c(-Inf, detlims, Inf)))

  x_is_nd <- as.numeric(cut_points) < max(as.numeric(cut_points))
  x_value <- ifelse(x_is_nd, detlims[cut_points], x)

  out <- cbind(time = x_value, status = !x_is_nd)

  return(structure(out, type = "left", class = "Surv"))
}


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
  out <- matrix(stats::rnorm(n_s * n_x), nrow = n_s, ncol = n_x)

  if (isTRUE(.scale)) {
    out <- structure(scale(out), "scaled:scale" = NULL, "scaled:center" = NULL)
  }

  tibble::as_tibble(out, .name_repair = \(.x) sprintf("x%02i", seq_along(.x)))
}
