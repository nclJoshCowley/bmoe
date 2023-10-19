#' MoE Patchwork Objects
#'
#' Creates a [`patchwork`][patchwork::patchwork-package] of `ggplot2` objects
#'   with partitioned data, requires the user to add [layers][get_mcmc_layer].
#'
#' @inheritParams extract_draws
#' @inheritParams get_mcmc_layer
#' @param new_data Passed to [calculate_log_lik]; ignored otherwise.
#' @param ... Extra arguments silently ignored.
#'
#' @name bmoe-plot
#' @aliases autoplot.bmoe_fit
#'
#' @export
autoplot.bmoe_fit <- function(object, varname, type = "none", ..., new_data) {
  if (length(type) > 1) {
    names(type) <- gsub("^Acf$", "ACF", tools::toTitleCase(type))

    out <- lapply(type, function(.type) autoplot(
      object, varname, .type, new_data = rlang::maybe_missing(new_data)
    ))

    if (varname %in% c("regr", "log_lik")) return(purrr::list_transpose(out))
    return(out)
  }

  rlang::check_dots_empty()
  cur_layer <- get_mcmc_layer(type)
  varname <- match.arg(varname, c(names(object$output), "log_lik"))

  switch(
    varname,
    regr = lapply(blank_patchwork_regr(object), `&`, cur_layer),
    wt = blank_patchwork_wt(object) & cur_layer,
    prec = blank_patchwork_prec(object) & cur_layer,
    log_lik = lapply(blank_patchwork_log_lik(object, new_data), `&`, cur_layer)
  )
}


#' @inheritParams bmoe-plot
#' @keywords internal
blank_patchwork_regr <- function(object) {
  regr_draws <- extract_draws(object, "regr")
  regr_draws_per_y <- split(regr_draws, regr_draws$y)

  plotlist_per_y <-
    lapply(regr_draws_per_y, function(.draws) {
      .draws |>
        dplyr::group_by(.data$.term, .data$x, .data$k) |>
        dplyr::group_map(.keep = TRUE, function(.x, .key) {
          ggplot2::ggplot(.x) +
            ggplot2::labs(subtitle = sprintf("%s, %s", .key$x, .key$k))
        })
    })

  n_x <- dim(object$output$regr)[3]

  lapply(plotlist_per_y, patchwork::wrap_plots, nrow = n_x, guides = "collect")
}


#' @inheritParams bmoe-plot
#' @keywords internal
blank_patchwork_wt <- function(object) {
  wt_draws <- extract_draws(object, "wt")

  plotlist <-
    wt_draws |>
    dplyr::group_by(.data$.term, .data$x, .data$k) |>
    dplyr::group_map(.keep = TRUE, function(.x, .key) {
      ggplot2::ggplot(.x) +
        ggplot2::labs(subtitle = sprintf("%s, %s", .key$x, .key$k))
    })

  n_x <- dim(object$output$wt)[3]

  patchwork::wrap_plots(plotlist, nrow = n_x, guides = "collect")
}


#' @inheritParams bmoe-plot
#' @keywords internal
blank_patchwork_prec <- function(object) {
  prec_draws <- extract_draws(object, "prec")

  plotlist <-
    prec_draws |>
    dplyr::group_by(.data$.term, .data$y, .data$k) |>
    dplyr::group_map(.keep = TRUE, function(.x, .key) {
      ggplot2::ggplot(.x) +
        ggplot2::labs(subtitle = sprintf("%s, %s", .key$y, .key$k))
    })

  n_y <- dim(object$output$prec)[3]

  patchwork::wrap_plots(plotlist, nrow = n_y, guides = "collect")
}


#' @inheritParams bmoe-plot
#' @keywords internal
blank_patchwork_log_lik <- function(object, new_data) {
  log_liks <- calculate_log_lik(object, new_data)

  purrr::imap(log_liks, function(.x, .nm) {
    ggplot2::ggplot(tidy(.x)) +
      ggplot2::labs(subtitle = sprintf("Log likelihood, %s", .nm))
  })
}
