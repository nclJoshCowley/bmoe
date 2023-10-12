#' Bayesian Mixture of Experts Linear Regression.
#'
#' Fits the model in JAGS.
#'
#' @param object object. A formula or simulated object.
#' @param data data frame. To be used in modelling.
#' @param prior named list. See **Prior** section.
#' @param jags_n named list. See **JAGS Controls** section.
#' @param inits list. Passed to `rjags::jags.model()`.
#' @inheritParams rlang::args_dots_empty
#'
#' @inheritSection bmoe-args Prior
#' @inheritSection bmoe-args JAGS Controls
#'
#' @aliases bmoe_fit
#' @export
bmoe <- function(object, ..., prior, jags_n = bmoe_jags_n(), inits = NULL) {
  UseMethod("bmoe")
}


#' @rdname bmoe
#' @export
bmoe.formula <- function(object, data, ..., prior,
                         jags_n = bmoe_jags_n(), inits = NULL) {

  formula <- parse_bmoe_formula(object)

  jags_n <- bmoe_jags_n(jags_n)

  mfs <- lapply(formula$regr, stats::model.frame, data = data)
  mf_wt <- stats::model.frame(formula$wt, data = data)

  y_list <- lapply(mfs, stats::model.response)
  x_regr <- stats::model.matrix(mfs[[1]], data = mfs[[1]])
  x_wt <- stats::model.matrix(mf_wt, data = mf_wt)

  out <- bmoe_yx(y_list, x_regr, x_wt, prior, jags_n, inits)

  out$prior <- prior
  out$inits <- inits
  out$formula <- formula

  out$data <- data

  return(out)
}


#' @rdname bmoe
#' @export
bmoe.bmoe_sim <- function(object, ..., prior,
                          jags_n = bmoe_jags_n(), inits = NULL) {

  y_nms <- grep("^y[0-9]+$", names(object$data), value = TRUE)
  y_sym <- lapply(y_nms, as.symbol)

  x_nms <- grep("^x[0-9]+$", names(object$data), value = TRUE)
  x_sym <- lapply(x_nms, as.symbol)

  formula <-
    rlang::new_formula(
      lhs = create_summand_from_symbols(y_sym),
      rhs = create_summand_from_symbols(x_sym)
    )

  out <-
    bmoe.formula(
      formula,
      data = object$data,
      # ...,
      prior = prior,
      jags_n = jags_n,
      inits = inits
    )

  out$params <- object$params
  out$new_data <- object$new_data

  class(out) <- c("bmoe_simstudy", class(out))

  return(out)
}


#' @noRd
#' @export
print.bmoe <- function(x, ...) {
  rlang::check_dots_empty()
  cat({ out <- format.bmoe(x) })
  invisible(out)
}


#' @noRd
#' @export
format.bmoe <- function(x, ...) {
  rlang::check_dots_empty()

  regr_formatted <- lapply(x$formula$regr, format)

  # The LHS of `wt` should be NULL but the LHS represents the latent components
  wt_representative_formula <- rlang::new_formula(quote(Pr(z[i] == k)), x$formula$wt[[2]])
  wt_formatted <- format(wt_representative_formula)

  paste(collapse = "\n\n", c(
    "Bayesian Mixture of Experts",
    sprintf("- `n_s = %s`", nrow(x$data)),
    sprintf("- `K = %s`", x$prior$k),
    "Regression structure",
    paste(
      sprintf("- `%s`", regr_formatted),
      collapse = "\n\n"
    ),
    "Weighting structure",
    sprintf("- `%s`", wt_formatted)
  ))
}


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

  for (nm in names(object$output)) {
    object$output[[nm]] <- bmoe_array(object$output[[nm]], varname = nm)
  }

  object$jags_n$n.iter <- length(iters)
  object$jags_n$n.chains <- length(chains)

  if (is.list(object$inits) && length(object$inits) == n_chains_pre) {
    object$inits <- object$inits[[chains]]
  }

  return(object)
}


#' Get Human Readable Names
#'
#' Helper to access names from data (`y`, `x`, `k`) to be displayed to users.
#'
#' @inheritParams bmoe-package
#'
#' @export
get_names_from_bmoe_fit <- function(object) {
  list(
    y =
      names(object$formula$regr),

    x =
      stats::model.frame(object$formula$regr[[1]], data = object$data) |>
      stats::model.matrix(data = object$data) |>
      colnames(),

    k =
      sprintf("k = %i", seq_len(object$prior$k))
  )
}


#' Get Regression Design Matrix (`x_regr`)
#'
#' Helper to access `x_regr` and verify a common RHS to regression formulas.
#'
#' @inheritParams bmoe-package
#'
#' @export
get_x_regr_from_bmoe_fit <- function(object, data) {
  mfs <- lapply(object$formula$regr, stats::model.frame, data = data)

  x_regr_list <- unique(lapply(mfs, stats::model.matrix, data = data))

  if (length(x_regr_list) > 1) stop("Regression formulas have different RHS")

  return(x_regr_list[[1]])
}
