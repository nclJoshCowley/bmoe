#' Apply Relabelling Scheme to [`bmoe`]
#'
#' Allows swapping of component labels, per MCMC iteration, to mitigate label
#'   switching.
#'
#' @inheritParams bmoe-package
#' @param permutations list. See **Permutations** section.
#'
#' @section Permutations:
#' Permutations are stored in a list where \eqn{i^{th}} elements corresponds to
#' the \eqn{i^{th}} chain.
#'
#' Each element is a matrix with one permutation per row for each iteration.
#' Note that
#' * A vector can be supplied to apply the same scheme to all MCMC output.
#' * A matrix can be supplied to apply the same relabelling to all chains.
#' * `NULL` can be supplied to use [boys_henderson_2002()].
#'
#' A permutation of `c(3, 1, 2)` denotes relabelling of
#' * `1 -> 3`
#' * `2 -> 1`
#' * `3 -> 2`
#'
#' @return [`bmoe_fit`] object with switched labels.
#'
#' @export
apply_relabelling <- function(object, permutations = NULL) {
  UseMethod("apply_relabelling")
}


#' @rdname apply_relabelling
#' @export
apply_relabelling.bmoe_fit <- function(object, permutations = NULL) {
  n_chains <- dim(object$output$regr)["chain"]
  n_iters <- dim(object$output$regr)["iteration"]

  if (is.null(permutations)) {
    permutations <- boys_henderson_2002(object$output$z, z_max = object$prior$k)
  }

  permutations <-
    validate_permutations(
      permutations,
      n_chains = n_chains,
      n_iters = n_iters
    )

  for (ic in seq_len(n_chains)) {
    for (ii in seq_len(n_iters)) {
      cur_perm <- permutations[[ic]][ii, ]

      # Permute allocation variable
      object$output$z[ii, ic, ] <- cur_perm[object$output$z[ii, ic, ]]

      # Permute weighting matrix
      object$output$wt[ii, ic, , ] <-
        object$output$wt[ii, ic, , ][, order(cur_perm)]

      # Re-implement corner constraint
      object$output$wt[ii, ic, , ] <-
        bmoe::sweep_ref_vals(object$output$wt[ii, ic, , ])

      # Permute regression matrix
      object$output$regr[ii, ic, , ,] <-
        object$output$regr[ii, ic, , ,][, , order(cur_perm)]

      # Permute precision parameter
      object$output$prec[ii, ic, , ] <-
        object$output$prec[ii, ic, , ][, order(cur_perm)]
    }
  }

  if (!is.null(attr(object, "permutations"))) {
    warning("Previous 'permutations' attribute has been overwritten")
  }

  return(structure(object, permutations = permutations))
}


#' Relabelling Algorithm, Boys & Henderson (2002)
#'
#' Relabelling algorithm detailed in paper "On determining the order of Markov
#'   dependence of an observed process" (Boys & Henderson, 2002).
#'
#' @param z `bmoe_array`. Component allocation output from [`bmoe_fit`].
#' @param start integer. Required starting point for this algorithm.
#' @param z_max integer. Maximum possible value of `z`.
#'
#' @return See **Permutations** section in [apply_relabelling()].
#'
#' @details Read paper for full details; aim is to minimise 'disagreement',
#'
#' \eqn{
#'    D
#'        = - \sum_{i = 1}^n \mathbb{I} \left(
#'            \nu_m(z_i^{(m)}) = \hat{z}_i^{(m - 1)}
#'        \right)
#' }
#'
#' where
#' * \eqn{\nu_m} is a candidate permutation to be chosen;
#' * \eqn{\hat{z}} is the marginal posterior mode of all MCMC iterations
#'   up to that point.
#' * relabelling is applied per MCMC iterations, \eqn{m = m_0, \dots, M} where
#'   the user defines \eqn{m_0} using the `start` argument.
#'
#' @references Boys, R. J. & Henderson, D. A. 2002
#'   On determining the order of markov dependence of an observed process
#'   governed by a hidden markov model.
#'   Scientific Programming 10 (3), 241–251.
#'
#' @export
boys_henderson_2002 <- function(z, start = 2, z_max = max(z)) {
  stopifnot(inherits(z, "bmoe_array"))
  n_chains <- dim(z)["chain"]
  n_iters <- dim(z)["iteration"]

  out <- rep(list(matrix(NA, nrow = n_iters, ncol = z_max)), n_chains)

  # Calculate K! permutations ahead of time
  all_perms <- permn(z_max)

  for (ic in seq_len(n_chains)) {
    message("\n", sprintf("Chain %i/%i", ic, n_chains))
    pb <- knitrProgressBar::progress_estimated(n_iters - start + 1)

    z_cur_chain <- z[, ic, , drop = TRUE]

    # Assume all permutations before `start` are identity
    for (ii in seq_len(start - 1)) out[[ic]][ii, ] <- seq_len(z_max)

    for (ii in seq(start, n_iters)) {
      knitrProgressBar::update_progress(pb)

      # Create / update estimate (using Marginal Posterior Mode)
      z_est <-
        apply(z_cur_chain[seq_len(ii - 1), , drop = FALSE], 2, arith_mode)

      # Obtain current iteration value for Z
      z_cur_draw <- z_cur_chain[ii, ]

      # Form list of all relabellings of Z at current iteration
      z_all_perms <- lapply(all_perms, function(.x) .x[z_cur_draw])

      # Choose permutation to minimise disagreement (cost)
      cur_cost <-
        vapply(z_all_perms, function(.z) -sum(.z == z_est), integer(1))

      cur_perm <- all_perms[[which.min(cur_cost)]]

      # Store current permutation to be returned
      out[[ic]][ii, ] <- cur_perm

      # Apply permutation to 'z' to be used in later calculations
      z[ii, ic, ] <- cur_perm[z_cur_draw]
    }
  }

  return(out)
}


#' @describeIn apply_relabelling Calculate summary of a `permutations` object.#'
#' @export
table_permutations <- function(permutations) {
  out <-
    lapply(permutations, function(.x) {
      counts <- sort(table(apply(.x, 1, toString)), decreasing = TRUE)

      # Use of `c(...)` removes harmful attributes generated by `table()`
      tibble::enframe(c(counts / nrow(.x)), name = "Permutation", "Frequency")
    })

  names(out) <- sprintf("Chain %i", seq_along(out))

  return(out)
}


#' Arithmetic Mode
#'
#' @param x vector. Assumed to be integer-like.
#' @param na.rm logical. Should `NA` be removed before calculation.
#'
#' @keywords internal
#' @noRd
arith_mode <- function(x, na.rm = FALSE) {
  if (na.rm) x = x[!is.na(x)]

  if (anyNA(x)) return(NA_real_)

  ux = unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


#' Generates all permutations of the elements of x
#'
#' See `combinat::permn` for author, references and implementation.
#'
#' @keywords internal
#' @noRd
permn <- function(x, fun = NULL, ...) {
  if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x) {
    x <- seq_len(x)
  }

  n <- length(x)
  nofun <- is.null(fun)
  out <- vector("list", gamma(n + 1))
  p <- ip <- seqn <- 1:n
  d <- rep(-1, n)
  d[1] <- 0
  m <- n + 1
  p <- c(m, p, m)
  i <- 1
  use <- -c(1, n + 2)
  while (m != 1) {
    out[[i]] <- if (nofun) x[p[use]] else fun(x[p[use]], ...)
    i <- i + 1
    m <- n
    chk <- (p[ip + d + 1] > seqn)
    m <- max(seqn[!chk])
    if (m < n) d[(m + 1):n] <- -d[(m + 1):n]
    index1 <- ip[m] + 1
    index2 <- p[index1] <- p[index1 + d[m]]
    p[index1 + d[m]] <- m
    tmp <- ip[index2]
    ip[index2] <- ip[m]
    ip[m] <- tmp
  }
  out
}


