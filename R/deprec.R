#' Deprecated Functions
#'
#' These functions should not be used and will be remove in `v1.0.0`.
#'
#' @name bmoe-deprec
NULL


#' @rdname bmoe-deprec
#' @export
calculate_x_regr <- function(object, new_data) {
  lifecycle::deprecate_warn(
    what = "calculate_x_regr()",
    when = "v1.0.0",
    details = "Please use `extract_x_regr()` instead"
  )

  extract_x_regr(object, new_data)
}
