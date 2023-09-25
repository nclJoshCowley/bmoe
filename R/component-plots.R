#' Visualise Component Allocation
#'
#' Show a raster plot where
#' * each row is an observation;
#' * each column is an individual MCMC iteration;
#' * each tile is coloured according to component membership.
#'
#' @inheritParams bmoe-package
#' @param chain integer. Specified chain, multiple changes forces list output.
#'
#' @export
component_allocation_visualisation <- function(object, chain) {
  invalid_chains <- setdiff(chain, seq_len(dim(object$output$z)["chain"]))

  if (length(invalid_chains)) {
    stop("Invalid chain arguments: ", toString(invalid_chains))
  }

  if (length(chain) > 1) {
    return(lapply(chain, component_allocation_visualisation, object = object))
  }

  k_labels <- get_names_from_bmoe_fit(object)$k

  z_draws <-
    tidy(object$output$z, .dimnames = "i") |>
    dplyr::mutate(.value = factor(.data$.value, labels = k_labels))

  mapping <- ggplot2::aes(x = .data$.iter, y = .data$i, fill = .data$.value)

  z_draws |>
    dplyr::filter(.data$.chain %in% chain) |>
    ggplot2::ggplot() +
    ggplot2::geom_raster(mapping = mapping) +
    ggplot2::labs(y = "Index", x = "Iterations") +
    ggplot2::scale_fill_discrete(name = NULL, drop = FALSE) +
    scale_x_iterations()
}
