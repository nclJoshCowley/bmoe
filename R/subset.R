#' Subset MCMC Output
#'
#' Subset the MCMC output and update details within a [`bmoe()`] fit.
#'
#' @inheritParams bmoe-package
#' @param iters integer. Vector of indices to keep, default includes all.
#' @param chains integer. Vector of chains to keep, default includes all.
#'
#' @export
subset_bmoe <- function(object, iters = NULL, chains = NULL) {
  stopifnot(inherits(object, "bmoe_fit"))

  n_chains_pre <- object$jags_n$n.chains

  if (is.null(iters)) iters <- seq_len(dim(object$output$regr)["iteration"])
  if (is.null(chains)) chains <- seq_len(dim(object$output$regr)["chain"])

  object$output$prec <- object$output$prec[iters, chains, , , drop = FALSE]
  object$output$regr <- object$output$regr[iters, chains, , , , drop = FALSE]
  object$output$wt <- object$output$wt[iters, chains, , , drop = FALSE]
  object$output$z <- object$output$z[iters, chains, , drop = FALSE]

  object$jags_n$n.iter <- length(iters)
  object$jags_n$n.chains <- length(chains)

  if (is.list(object$inits) && length(object$inits) == n_chains_pre) {
    object$inits <- object$inits[[chains]]
  }

  return(object)
}
