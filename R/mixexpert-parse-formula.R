#' Parse MoE Formula
#'
#' Internal method to allow multiple response and multiple RHS within
#'   a [`formula`][stats::formula].
#'
#' @section Extended Formula:
#' For more information on extending `formula`, see the `Formula` package but
#' be warned about <https://github.com/rstudio/rstudio/issues/12409>.
#'
#' We allow the user to jointly model multiple response variables using `+`:
#' * `y01 + y02 ~ x01 + x02`
#' * `y01 + y02 ~ .`
#'
#' The RHS can also be split into two parts using `|` where the former is used
#'   for regression and the latter is used for weighting.
#' A formula with only one part asserts a single design matrix for both.
#' * `y01 ~ x01 + x02 | x03`
#' * `y01 + y02 ~ . | x01`
#'
#' @param object formula. User supplied model description.
#'
#' @aliases mixexpert-formula
#' @keywords internal
parse_mixexpert_formula <- function(object) {
  stopifnot(inherits(object, "formula"))
  is_lhs_empty <- length(object) == 2

  lhs <- if (is_lhs_empty) NULL else object[[2]]
  rhs <- if (is_lhs_empty) object[[2]] else object[[3]]

  rhs_formulas <- split_call(rhs, "|")
  stopifnot("Bad number of parts to RHS" = length(rhs_formulas) %in% c(1, 2))

  rhs_regr <- rhs_formulas[[1]]
  rhs_wt <-
    if (length(rhs_formulas) == 1) rhs_formulas[[1]] else rhs_formulas[[2]]

  formula_wt <- rlang::new_formula(NULL, rhs_wt)

  formula_regr <-
    lapply(split_call(lhs, "+"), rlang::new_formula, rhs = rhs_regr)

  list(regr = formula_regr, wt = formula_wt)
}


#' Splits Call into List
#'
#' @seealso Code sourced from function body of `Formula:::split_formula`.
#' @keywords internal
split_call <- function(x, sep = "|") {
  if (is.null(x)) return(list(NULL))

  rval <- list()
  if (length(x) > 1L && x[[1L]] == sep) {
    while (length(x) > 1L && x[[1L]] == sep) {
      rval <- c(x[[3L]], rval)
      x <- x[[2L]]
    }
  }
  return(c(x, rval))
}


#' Create Call From (List of) Symbols
#'
#' Output will be `expression(..1 + ..2)` based on input `list(..1, ..2)`.
#'
#' @param x list. Expressions to be added together.
#' @keywords internal
create_summand_from_symbols <- function(x) {
  stopifnot(length(x) > 1)

  out <- x[[1]]
  for (i in seq(from = 2, to = length(x))) {
    out <- rlang::expr(!!out + !!x[[i]])
  }

  return(out)
}
