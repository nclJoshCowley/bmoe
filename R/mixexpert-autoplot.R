#' MoE Patchwork Objects
#'
#' Creates a [`patchwork`][patchwork::patchwork-package] of `ggplot2` objects
#'   with partitioned data, requires the user to add [layers][get_layer].
#'
#' @inheritParams bmoe-package
#' @param varname character. Single variable name such as `regr` or `prec`.
#' @inheritParams get_layer
#' @inheritParams rlang::args_dots_empty
#'
#' @name mixexpert-plot
NULL


#' @rdname mixexpert-plot
#' @export
autoplot.mixexpert <- function(object, varname, type = "none", ...) {
  cur_layer <- get_layer(type)

  rlang::check_dots_empty()
  varname <- match.arg(varname, names(object$output))

  switch(
    varname,
    regr = lapply(autoplot_regr(object), `&`, cur_layer),
    prec = autoplot_prec(object) & cur_layer,
    wt = autoplot_wt(object) & cur_layer
  )
}


#' @inheritParams mixexpert-plot
#' @keywords internal
autoplot_regr <- function(object) {
  nms <- get_names_from_mixexpert(object)
  n_x <- length(nms$x)

  regr_draws <-
    tidy(object$output$regr) |>
    dplyr::mutate(extract_term_indices(.data$.term, c("x", "y", "k")))

  regr_draws_per_y <-
    split(regr_draws, factor(regr_draws$y, labels = nms$y))

  plotlist_per_y <-
    lapply(regr_draws_per_y, function(.draws) {
      .draws |>
        dplyr::group_by(.data$.term, .data$x, .data$k) |>
        dplyr::group_map(function(.x, .key) {
          subtitle <- sprintf("%s, %s", nms$x[.key$x], nms$k[.key$k])
          ggplot2::ggplot(.x) + ggplot2::labs(subtitle = subtitle)
        })
    })

  lapply(plotlist_per_y, function(.x) {
    patchwork::wrap_plots(.x, nrow = n_x, guides = "collect")
  })
}


#' @inheritParams mixexpert-plot
#' @keywords internal
autoplot_prec <- function(object) {
  nms <- get_names_from_mixexpert(object)
  n_y <- length(nms$y)

  prec_draws <-
    tidy(object$output$prec) |>
    dplyr::mutate(extract_term_indices(.data$.term, c("y", "k")))

  plotlist <-
    prec_draws |>
    dplyr::group_by(.data$.term, .data$y, .data$k) |>
    dplyr::group_map(function(.x, .key) {
      subtitle <- sprintf("%s, %s", nms$y[.key$y], nms$k[.key$k])
      ggplot2::ggplot(.x) + ggplot2::labs(subtitle = subtitle)
    })

  patchwork::wrap_plots(plotlist, nrow = n_y, guides = "collect")
}


#' @inheritParams mixexpert-plot
#' @keywords internal
autoplot_wt <- function(object) {
  nms <- get_names_from_mixexpert(object)
  n_x <- length(nms$x)

  wt_draws <-
    tidy(object$output$wt) |>
    dplyr::mutate(extract_term_indices(.data$.term, c("x", "k")))

  plotlist <-
    wt_draws |>
    dplyr::group_by(.data$.term, .data$x, .data$k) |>
    dplyr::group_map(function(.x, .key) {
      subtitle <- sprintf("%s, %s", nms$x[.key$x], nms$k[.key$k])
      ggplot2::ggplot(.x) + ggplot2::labs(subtitle = subtitle)
    })

  patchwork::wrap_plots(plotlist, nrow = n_x, guides = "collect")
}


#' Extract Term Indices
#'
#' Convert term, say `regr[a, b, c]`, to df with column for each dimension.
#'
#' @param x character vector. Matches `foo[...]` where only `...` are used.
#' @param dnames character vector. Output column names.
#'
#' @keywords internal
extract_term_indices <- function(x, dnames = NULL) {
  # Recall
  # * (?<=_) / (?=_) means look behind / ahead for '_' but don't include it
  cur_pattern <- "(?<=\\[)[0-9,]+(?=\\]$)"
  term_info_str <- regmatches(x, regexpr(cur_pattern, x, perl = TRUE))

  out <-
    term_info_str |>
    strsplit(",", fixed = TRUE) |>
    purrr::list_transpose() |>
    lapply(as.integer) |>
    tibble::as_tibble(.name_repair = "minimal")

  colnames(out) <- dnames %||% sprintf(".dim%i", seq_len(ncol(out)))

  return(out)
}
