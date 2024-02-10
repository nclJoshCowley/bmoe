#' Get Data and Parameter Dimensions
#'
#' Helper to access names from data (`y`, `x`, `k`) to be displayed to users.
#'
#' @inheritParams bmoe-package
#' @param id choice. One of
#'   * `n_iters`: Number of iterations.
#'   * `n_chains`: Number of chains.
#'   * `n_s`: Number of observations.
#'   * `n_y`: Number of response variables.
#'   * `n_k`: Assumed number of components.
#'   * `p_regr`: Number of regression predictors (including intercept).
#'   * `p_wt`: Number of weighting predictors (including intercept).
#'
#' @keywords internal
get_dims_from_bmoe_fit <- function(object, id) {
  valid_choices <-
    c("n_iters", "n_chains",  "n_s", "n_y", "n_k", "p_regr", "p_wt")

  switch(
    match.arg(id, choices = valid_choices),
    n_iters = unname(dim(object$output$wt))[1],
    n_chains = unname(dim(object$output$wt))[2],
    n_s = nrow(object$data),
    n_y = length(object$formula$regr),
    n_k = unname(dim(object$output$regr))[5],
    p_regr = unname(dim(object$output$regr))[3],
    p_wt = unname(dim(object$output$wt))[3]
  )
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
