#' Bayesian Mixture of Experts Package
#'
#' @description
#'   Bayesian Mixture of Experts (MoE) applied to possibly left-censored
#'   linear regression.
#'
#' @param object Object that inherits from [`bmoe_fit`].
#'
#' @param new_data data frame.
#'   Similar structure to original `data` argument.
#'
#'   Setting `new_data = NULL` signals for the observed data to be used.
#'
#' @docType package
#' @name bmoe-package
#'
#' @section Model Specification:
#' TODO
NULL


#' @keywords internal
#' @noRd
check_new_data <- function(object, new_data) {
  if (missing(new_data)) {
    stop("Argument `new_data` must be provided. See ?`bmoe-package`")
  }

  if (is.null(new_data)) return(object$data)

  return(new_data)
}
