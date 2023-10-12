#' Visualise Component Allocation
#'
#' Show a raster plot where
#' * each row is an observation;
#' * each column is an individual MCMC iteration;
#' * each tile is coloured according to component membership.
#'
#' @inheritParams bmoe-package
#' @param chain integer. Specified chain(s), multiple chains forces list output.
#'
#' @export
component_allocation_visualisation <- function(object, chain) {
  k_labels <- get_names_from_bmoe_fit(object)$k

  if (length(chain) == 1) {
    return(impl_c_alloc_vis(drop(object$output$z[, chain, ]), k_labels))
  }

  lapply(asplit(object$output$z, 2), impl_c_alloc_vis, labels = k_labels)
}


#' Implementation of `component_allocation_visualisation`
#'
#' Create `ggplot2` object on a chain-by-chain basis to help with `gc()`.
#'
#' @param x integer-valued matrix. Rows (columns) denote iterations (indices).
#' @param labels character. Labels to use in plot and `$data`.
#'
#' @keywords internal
impl_c_alloc_vis <- function(x, labels) {
  plot_data <-
    expand.grid(.iter = seq_len(nrow(x)), i = seq_len(ncol(x)))

  plot_data$.value <- structure(c(x), levels = labels, class = "factor")

  plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_raster(ggplot2::aes(
      x = .data$.iter, y = .data$i, fill = .data$.value
    )) +
    ggplot2::labs(y = "Index", x = "Iterations", fill = NULL) +
    ggplot2::scale_fill_discrete(name = NULL, drop = FALSE) +
    scale_x_iterations()
}
