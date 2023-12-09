#' Visualise Posterior Group Membership, Ordered by Rank
#'
#' Combination of allocation probabilities and quantile plots
#'
#' @details
#' For consistency between each plot, ordering is based on a single chosen
#'    response variable.
#'
#' @note Both `.plot_by` and `.rank_by` can be censored (`<Surv>`).
#'
#' @inheritParams bmoe-package
#' @param .chain integer. Which chain indexes to use as posterior allocations.
#' @param .rank_by expression. Column to use in ordering (y-axis).
#' @param .plot_by expression. Column to be plotted (x-axis).
#'
#' @name bmoe-allocation-vis
NULL


#' @rdname bmoe-allocation-vis
#' @export
visualise_allocations_with_response_data <- function(object, .chain, .rank_by) {
  .rank_by <- rlang::enexpr(.rank_by)

  quantile_plotlist <-
    object$formula$regr |>
    lapply(function(.formula) .formula[[2]]) |>
    lapply(function(.y) visualise_response_data(object, {{.y}}, {{.rank_by}}))

  allocation_plot <-
    visualise_allocation_data(object, .chain)

  allocation_plot$data <-
    dplyr::left_join(
      allocation_plot$data,
      quantile_plotlist[[1]]$data[c(".index", ".rank")],
      by = ".index"
    )

  allocation_plot$mapping$y <- quote(.data$.rank)
  allocation_plot$labels$y <- quantile_plotlist[[1]]$labels$y

  patchwork::wrap_plots(
    c(list(allocation_plot), quantile_plotlist),
    nrow = 1,
    guides = "collect"
  )
}


#' Visualise Posterior Allocation Data
#'
#' @section Posterior Allocation Plot:
#'   Histogram showing the corresponding proportions of each component
#'   allocation per observation.
#'
#' @rdname bmoe-allocation-vis
#' @export
visualise_allocation_data <- function(object, .chain) {
  draws <-
    bmoe::extract_draws(object, "z") |>
    dplyr::rename(.index = "i") |>
    dplyr::filter(.data$.chain %in% .env$.chain) |>
    dplyr::mutate(
      .term = factor(.data$.term, unique(.data$.term)),
      .value = factor(
        .data$.value,
        levels = seq_len(object$prior$k),
        labels = sprintf("k = %i", seq_len(object$prior$k))
      )
    )

  stopifnot("No draws found for these chains" = nrow(draws) > 0)

  draws |>
    ggplot2::ggplot(ggplot2::aes(y = .data$.index, fill = .data$.value)) +
    ggplot2::geom_bar(
      width = 1,
      position = ggplot2::position_fill(vjust = 0.5)
    ) +
    ggplot2::scale_fill_discrete(name = NULL, drop = FALSE) +
    ggplot2::labs(y = "Index", x = NULL)
}



#' Visualise Response Data
#'
#' @section: Quantile Plot:
#'   Response data is shown by a quantile plot where the y-axis "rank" reflects
#'   rank of an arbitrary dependent variable.
#'
#' @rdname bmoe-allocation-vis
#' @export
visualise_response_data <- function(object, .plot_by, .rank_by) {
  y_data <-
    lapply(object$formula$regr, function(.x) {
      stats::model.response(stats::model.frame(.x, data = object$data))
    }) |>
    dplyr::bind_cols()

  is_plot_cens <- inherits(dplyr::pull(y_data, {{.plot_by}}), "Surv")
  is_rank_cens <- inherits(dplyr::pull(y_data, {{.rank_by}}), "Surv")

  get_value <- function(.x) .x[, "time"]
  get_is_nd <- function(.x) !(.x[, "status"])

  plot_data <-
    y_data |>
    dplyr::mutate(
      .index = seq_len(dplyr::n()),
      .rank = rank(
        if (is_rank_cens) get_value({{.rank_by}}) else {{.rank_by}},
        na.last = "keep",
        ties.method = "first"
      ),
      .before = 1
    ) |>
    dplyr::mutate(
      .value = if (is_plot_cens) get_value({{.plot_by}}) else {{.plot_by}},
      .is_nd = if (is_plot_cens) get_is_nd({{.plot_by}}) else FALSE,
      .keep = "unused"
    )

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      y = .data$.rank, x = .data$.value, shape = .data$.is_nd
    )) +
    ggplot2::geom_point() +
    ggplot2::labs(
      y = sprintf("Rank (%s)", deparse(.rank_by)),
      x = deparse(.plot_by)
    ) +
    ggplot2::scale_shape_manual(
      name = NULL,
      values = c(`TRUE` = 1, `FALSE` = 16),
      labels = c(`TRUE` = "Censored", `FALSE` = "Observed")
    )

}
