#' Prediction for Bayesian Mixture of Expert Fits
#'
#' Both response and component membership can be generated.
#'
#' @inheritParams bmoe-package
#' @param ... Extra arguments ignored.
#' @param type choice. Determines output type
#'
#' @returns
#' * `type = "numeric"`. Tibble output with 1 MCMC list column per response.
#' * `type = raw`. A [`bmoe_array`] MCMC object is returned.
#' * `type = class`. The component membership, `z`, is returned.
#'
#' @export
predict.bmoe_fit <- function(object, ..., new_data, type) {
  type <- match.arg(type, c("numeric", "raw", "class"))

  z <- calculate_component_samples(object, new_data)

  if (type == "class") return(bmoe_array_to_pred_tbl(z, ".pred_class"))

  y_mean <- calculate_posterior_y_mean(object, new_data, z, separate = FALSE)
  y_sd <- calculate_posterior_y_sd(object, z, separate = FALSE)

  y_pred <-
    array(
      stats::rnorm(n = length(y_mean), mean = y_mean, sd = y_sd),
      dim = dim(y_mean)
    )

  if (type == "raw") return(bmoe_array(y_pred, varname = "y_pred"))

  y_keys <- sprintf(".pred_%s", get_names_from_bmoe_fit(object)$y)

  return(dplyr::bind_cols(
    Map(bmoe_array_to_pred_tbl, asplit(y_pred, 4), .key = as.list(y_keys))
  ))
}


#' @keywords internal
#' @noRd
bmoe_array_to_pred_tbl <- function(x, .key) {
  x |>
    tidy.bmoe_array() |>
    dplyr::group_by(.data$.term) |>
    dplyr::group_nest(.key = .key) |>
    dplyr::select(-".term")
}
