#' Extract Data from Model Fit
#'
#' Getter functions for the data used in a `bmoe` model fit.
#'
#' @inheritParams bmoe-package
#'
#' @name bmoe-extract-data
NULL


#' @rdname bmoe-extract-data
#' @export
extract_x_regr <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object$data

  x_regr_list <-
    unique(
      lapply(object$formula$regr, function(.form) {
        .form |>
          stats::terms() |>
          stats::delete.response() |>
          stats::model.frame(data = new_data) |>
          stats::model.matrix(data = new_data)
      })
    )

  if (length(x_regr_list) > 1) stop("Regression formulas have different RHS")

  return(x_regr_list[[1]])
}


#' @rdname bmoe-extract-data
#' @export
extract_x_wt <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object$data

  object$formula$wt |>
    stats::terms() |>
    stats::delete.response() |>
    stats::model.frame(data = new_data) |>
    stats::model.matrix(data = new_data)
}


#' @rdname bmoe-extract-data
#'
#' @returns `extract_y()` returns a matrix with one column per `y` variable.
#'
#' @export
extract_y <- function(object, new_data) {
  new_data <- check_new_data(object, new_data)

  mfs <- lapply(object$formula$regr, stats::model.frame, data = new_data)
  y_list <- lapply(mfs, stats::model.response)

  return(do.call(cbind, y_list))
}
