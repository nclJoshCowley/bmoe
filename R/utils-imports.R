utils::globalVariables(".")
utils::globalVariables("!<-")


#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' @importFrom generics tidy
#' @export
generics::tidy


#' Imports
#'
#' Imports from other packages used within this package
#'
#' @name utils-imports
#'
#' @importFrom rlang .data .env
#' @importFrom rlang %||%
#'
#' @keywords internal
#'
#' @section Links:
#'   - [`rlang::dot-data()`], data pronoun.
#'   - [`rlang::op-null-default()`], default value for NULL operator.
NULL
