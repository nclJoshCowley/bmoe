#' Extract Posterior Samples from Bayesian Mixture of Experts
#'
#' @inheritParams bmoe-package
#' @param varname character. Variable name defined in the model.
#' @inheritParams rlang::args_dots_empty
#'
#' @section Simulation Study:
#' For models fit to a `bmoe_sim` object, a `$.truth` column may also be added.
#'
#' This functionality is conditional on the assumed \eqn{K} in `prior` being
#'   the same as the \eqn{K} used to simulate the data.
#'
#' @export
extract_draws <- function(object, varname, ...) {
  UseMethod("extract_draws")
}


#' @rdname extract_draws
#' @export
extract_draws.bmoe_fit <- function(object, varname, ...) {
  rlang::check_dots_empty()

  varname <- match.arg(varname, names(object$output))

  include_nms <- getOption("bmoe.include_labels_with_draws", default = TRUE)
  nms <- get_names_from_bmoe_fit(object)

  out <-
    switch(
      varname,
      regr = tidy(object$output$regr, .dimnames = c("x", "y", "k")),
      wt = tidy(object$output$wt, .dimnames = c("x", "k")),
      prec = tidy(object$output$prec, .dimnames = c("y", "k")),
      z = tidy(object$output$z, .dimnames = c("i"))
    )

  if (isFALSE(include_nms)) return(out)

  out |>
    dplyr::mutate(
      y = if ("y" %in% names(out)) factor(.data$y, labels = nms$y) else NULL,
      x = if ("x" %in% names(out)) factor(.data$x, labels = nms$x) else NULL,
      k = if ("k" %in% names(out)) factor(.data$k, labels = nms$k) else NULL
    )
}


#' @rdname extract_draws
#' @export
extract_draws.bmoe_simstudy <- function(object, varname, ...) {
  out <- NextMethod()

  is_fit_k_correct <- dim(object$params$regr)[3] != object$prior$k
  if (!is_fit_k_correct) return(out)

  vardim <- utils::tail(unname(dim(object$output[[varname]])), -2)

  indices_df <- do.call(expand.grid, lapply(vardim, seq_len))
  indices <- apply(indices_df, 1, function(.x) paste(.x, collapse = ","))

  truth_tbl <-
    tibble::tibble(
      .term = sprintf("%s[%s]", varname, indices),
      .truth = c(object$params[[varname]])
    )

  return(dplyr::left_join(out, truth_tbl, by = ".term"))
}
