#' MoE Patchwork Objects
#'
#' Creates a [`patchwork`][patchwork::patchwork-package] of `ggplot2` objects
#'   with partitioned data, requires the user to add [layers][get_mcmc_layer].
#'
#' @inheritParams bmoe-package
#' @param varname character. Variable name defined in the model.
#' @inheritParams get_mcmc_layer
#' @param new_data Passed to [calculate_log_lik]; ignored otherwise.
#' @param ... Extra arguments silently ignored.
#'
#' @name bmoe-plot
NULL


#' @rdname bmoe-plot
#' @export
autoplot.bmoe_fit <- function(object, varname, type = "none", ..., new_data) {
  if (length(type) > 1) {
    names(type) <- gsub("^Acf$", "ACF", tools::toTitleCase(type))

    out <- lapply(type, function(.type) {
      autoplot(
        object, varname, .type, new_data = rlang::maybe_missing(new_data)
      )
    })

    is_split_by_y_nms <- varname %in% c("regr", "log_lik")
    return(if (is_split_by_y_nms) purrr::list_transpose(out) else out)
  }

  cur_layer <- get_mcmc_layer(type)

  rlang::check_dots_empty()
  varname <- match.arg(varname, c(names(object$output), "log_lik"))

  switch(
    varname,
    regr = lapply(autoplot_regr(object), `&`, cur_layer),
    wt = autoplot_wt(object) & cur_layer,
    prec = autoplot_prec(object) & cur_layer,
    log_lik = lapply(autoplot_log_lik(object, new_data), `&`, cur_layer)
  )
}


#' @inheritParams bmoe-plot
#' @keywords internal
autoplot_regr <- function(object) {
  nms <- get_names_from_bmoe_fit(object)
  n_x <- length(nms$x)

  regr_draws <- tidy(object$output$regr, .dimnames = c("x", "y", "k"))

  object$params$regr <- tibble::enframe(object$params$regr)

  regr_draws_per_y <-
    split(regr_draws, factor(regr_draws$y, labels = nms$y))

  plotlist_per_y <-
    lapply(regr_draws_per_y, function(.draws) {
      .draws |>
        dplyr::group_by(.data$.term, .data$x, .data$k) |>
        dplyr::group_map(
          function(.x, .key) {
            subtitle <- sprintf("%s, %s", nms$x[.key$x], nms$k[.key$k])
            ggplot2::ggplot(.x) + ggplot2::labs(subtitle = subtitle)
          },
          .keep = TRUE
        )
    })

  lapply(plotlist_per_y, function(.x) {
    patchwork::wrap_plots(.x, nrow = n_x, guides = "collect")
  })
}


#' @inheritParams bmoe-plot
#' @keywords internal
autoplot_wt <- function(object) {
  nms <- get_names_from_bmoe_fit(object)
  n_x <- length(nms$x)

  wt_draws <- tidy(object$output$wt, .dimnames = c("x", "k"))

  plotlist <-
    wt_draws |>
    dplyr::group_by(.data$.term, .data$x, .data$k) |>
    dplyr::group_map(
      function(.x, .key) {
        subtitle <- sprintf("%s, %s", nms$x[.key$x], nms$k[.key$k])
        ggplot2::ggplot(.x) + ggplot2::labs(subtitle = subtitle)
      },
      .keep = TRUE
    )

  patchwork::wrap_plots(plotlist, nrow = n_x, guides = "collect")
}


#' @inheritParams bmoe-plot
#' @keywords internal
autoplot_prec <- function(object) {
  nms <- get_names_from_bmoe_fit(object)
  n_y <- length(nms$y)

  prec_draws <- tidy(object$output$prec, .dimnames = c("y", "k"))

  plotlist <-
    prec_draws |>
    dplyr::group_by(.data$.term, .data$y, .data$k) |>
    dplyr::group_map(
      function(.x, .key) {
        subtitle <- sprintf("%s, %s", nms$y[.key$y], nms$k[.key$k])
        ggplot2::ggplot(.x) + ggplot2::labs(subtitle = subtitle)
      },
      .keep = TRUE
    )

  patchwork::wrap_plots(plotlist, nrow = n_y, guides = "collect")
}


#' @inheritParams bmoe-plot
#' @keywords internal
autoplot_log_lik <- function(object, new_data) {
  log_liks <- calculate_log_lik(object, new_data)

  purrr::imap(log_liks, function(.x, .nm) {
    ggplot2::ggplot(tidy(.x)) +
      ggplot2::labs(subtitle = sprintf("Log likelihood, %s", .nm))
  })
}
