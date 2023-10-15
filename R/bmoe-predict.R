#' Prediction for Bayesian Mixture of Expert Fits
#'
#' Both response and component membership can be generated.
#'
#' @inheritParams bmoe-package
#' @param ... Extra arguments ignored.
#' @param type choice. Determines output type.
#' @param summarise logical. When `TRUE`, any list column output is summarised.
#'
#' @returns
#' * `type = "response"`. Tibble output with a list column per response.
#' * `type = "class"`. Component membership, `z`, is returned as a list column.
#' * `type = "raw"`. A [`bmoe_array`] MCMC object is returned.
#'
#' @export
predict.bmoe_fit <- function(object, ..., new_data, type, summarise = TRUE) {
  if (length(type) > 1) {
    return(dplyr::bind_cols(
      lapply(type, function(.type) {
        predict.bmoe_fit(
          object, new_data = new_data, type = .type, summarise = summarise
        )
      })
    ))
  }

  type <- match.arg(type, c("response", "raw", "class"))
  y_keys <- sprintf(".pred_%s", get_names_from_bmoe_fit(object)$y)

  z <- calculate_component_samples(object, new_data)

  if (type == "class") {
    if (summarise) {
      return(bmoe_array_to_pred_tb_summary(z, ".pred_class"))
    } else {
      return(bmoe_array_to_pred_tb(z, ".pred_class"))
    }
  }

  y_mean <- calculate_posterior_y_mean(object, new_data, z, separate = FALSE)
  y_sd <- calculate_posterior_y_sd(object, z, separate = FALSE)

  y_pred <-
    array(
      stats::rnorm(n = length(y_mean), mean = y_mean, sd = y_sd),
      dim = dim(y_mean)
    )

  y_pred <- bmoe_array(y_pred, varname = "y_pred")

  if (type == "response") {
    if (summarise) {
      return(bmoe_array_to_pred_tb_summary(y_pred, y_keys))
    } else {
      return(bmoe_array_to_pred_tb(y_pred, y_keys))
    }
  }

  # type = "raw" returns both 'response' and 'class'
  return(list(y_pred = y_pred, z = z))
}


#' Convert `bmoe_array` to Prediction Table
#'
#' Creates a list column per response (or class) where each element is a draws
#'   tibble containing `.iter`, `.chain` and `.value` for a single parameter.
#'
#' @param x [`bmoe_array`] object.
#' @param .key character. Names to use in output, length should be `dim(x)[4]`.
#'
#' @keywords internal
bmoe_array_to_pred_tb <- function(x, .key) {
  if (length(dim(x)) == 4) {
    separated_x <- lapply(asplit(x, 4), bmoe_array, varname = NULL)
    return(dplyr::bind_cols(
      Map(bmoe_array_to_pred_tb, x = separated_x, .key = as.list(.key))
    ))
  }

  out <-
    tidy.bmoe_array(x) |>
    dplyr::group_by(.data$.term) |>
    dplyr::group_nest(.key = .key) |>
    dplyr::select(-".term")

  return(out)
}


#' @describeIn bmoe_array_to_pred_tb
#'   Wrapper that further summarises the output per chain.
#'
#' @keywords internal
#' @noRd
bmoe_array_to_pred_tb_summary <- function(x, .key) {
  pred_tb <- bmoe_array_to_pred_tb(x, .key)
  is_nominal <- identical(.key, ".pred_class")

  pred_tb |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.key),
        function(.l) lapply(.l, summarise_bare_draws(is_nominal))
      )
    )
}


#' Summarise Draws for a Single Parameter
#'
#' Assumes `.draws` contains `.iter`, `.chain`, `.value`) for single parameter.
#'
#' @keywords internal
#' @noRd
summarise_bare_draws <- function(is_nominal) {
  if (is_nominal) {
    function(.draws) {
      dplyr::summarise(
        dplyr::group_by(.draws, .data$.chain),
        mode = arith_mode(.data$.value)
      )
    }
  } else {
    function(.draws) {
      dplyr::summarise(
        dplyr::group_by(.draws, .data$.chain),
        mean = mean(.data$.value),
        lower = stats::quantile(.data$.value, probs = c(0.025)),
        upper = stats::quantile(.data$.value, probs = c(0.975))
      )
    }
  }
}
