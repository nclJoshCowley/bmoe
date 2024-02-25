#' List of MoE Analysis Plot Objects
#'
#' Creates a nested list of `ggplot2` objects with partitioned data, requires
#'   the user to add [layers][mcmc_layer].
#'
#' @inheritParams extract_draws
#' @inheritParams mcmc_layer
#' @param new_data Passed to [`extract_log_lik()`]; ignored otherwise.
#' @param ... Extra arguments silently ignored.
#'
#' @name bmoe-plot
#' @aliases autoplot.bmoe_fit
#'
#' @export
autoplot.bmoe_fit <- function(object, varname, type = "none", ..., new_data) {
  if (is.character(type) && length(type) > 1) {
    new_data <- rlang::maybe_missing(new_data)

    out <-
      type |>
      rlang::set_names(gsub("^Acf$", "ACF", tools::toTitleCase(type))) |>
      lapply(function(.x) autoplot(object, varname, .x, new_data = new_data))

    return(out)
  }
  rlang::check_dots_empty()
  varname <- match.arg(varname, c(names(object$output), "log_lik"))

  is_fit_k_correct <- dim(object$params$regr)[3] == object$prior$k
  is_truth_shown <-
    inherits(object, "bmoe_simstudy") &&
    is_fit_k_correct &&
    (varname != "log_lik")

  cur_layer <- mcmc_layer(type, .truth = is_truth_shown)

  switch(
    varname,
    regr = make_empty_plotlist_regr(object) |>
      purrr::modify_depth(2, `&`, cur_layer),

    wt = make_empty_plotlist_wt(object) |>
      purrr::modify_depth(1, `&`, cur_layer),

    prec = make_empty_plotlist_prec(object) |>
      purrr::modify_depth(1, `&`, cur_layer),

    log_lik = make_empty_plot_log_lik(object, new_data) + cur_layer,
  )
}


#' @inheritParams bmoe-plot
#' @keywords internal
make_empty_plotlist_regr <- function(object) {
  regr_draws <- extract_draws(object, "regr")
  regr_draws_per_y <- split(regr_draws, regr_draws$y)

  lapply(regr_draws_per_y, function(.draws) {
    .draws |>
      dplyr::group_by(.data$.term, .data$x, .data$k) |>
      dplyr::group_map(.keep = TRUE, function(.x, .key) {
        ggplot2::ggplot(.x) +
          ggplot2::labs(subtitle = sprintf("%s, %s", .key$x, .key$k))
      })
  })
}


#' @inheritParams bmoe-plot
#' @keywords internal
make_empty_plotlist_wt <- function(object) {
  wt_draws <- extract_draws(object, "wt")

  wt_draws |>
    dplyr::group_by(.data$.term, .data$x, .data$k) |>
    dplyr::group_map(.keep = TRUE, function(.x, .key) {
      ggplot2::ggplot(.x) +
        ggplot2::labs(subtitle = sprintf("%s, %s", .key$x, .key$k))
    })
}


#' @inheritParams bmoe-plot
#' @keywords internal
make_empty_plotlist_prec <- function(object) {
  prec_draws <- extract_draws(object, "prec")

  prec_draws |>
    dplyr::group_by(.data$.term, .data$y, .data$k) |>
    dplyr::group_map(.keep = TRUE, function(.x, .key) {
      ggplot2::ggplot(.x) +
        ggplot2::labs(subtitle = sprintf("%s, %s", .key$y, .key$k))
    })
}


#' @inheritParams bmoe-plot
#' @keywords internal
make_empty_plot_log_lik <- function(object, new_data) {
  log_liks <- extract_log_lik(object, new_data)

  ggplot2::ggplot(tidy(log_liks)) +
    ggplot2::labs(subtitle = "Log likelihood")
}


#' Get `ggplot2` Layer(s) for Plotting MCMC Output.
#'
#' @param type choice. Name of the supported plot type including
#' * `"none"`, plots are to be empty until the user adds layers.
#' * `"density"`, useful distribution visualisation.
#' * `"trace"`, useful for verifying chain convergence.
#' * `"acf"`, autocorrelation of the MCMC samples at discrete lags.
#' Alternatively one can pass a **list** of custom layers to be added.
#'
#' @name mcmc_layer
NULL


#' @rdname mcmc_layer
#' @export
mcmc_layer <- function(type, .truth = FALSE) {
  if (is.list(type) || inherits(type, "Layer")) return(type)

  switch(
    match.arg(type, c("none", "density", "trace", "acf")),
    none = list(),
    density = layer_density(.truth),
    trace = layer_trace(.truth),
    acf = layer_acf()
  )
}


#' @rdname mcmc_layer
#' @keywords internal
layer_density <- function(.truth = FALSE) {
  cur_labs <- ggplot2::labs(y = NULL, x = NULL, fill = "Chain")

  cur_geom_density <-
    ggplot2::geom_density(
      ggplot2::aes(
        x = .data$.value,
        fill = factor(.data$.chain)
      ),
      adjust = 1.5,
      alpha = 0.1,
    )

  cur_geom_vline <-
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data$.truth),
      linetype = "dashed",
      alpha = 0.55
    )

  out <- list(cur_labs, cur_geom_density)
  return(if (.truth) append(out, cur_geom_vline) else out)
}


#' @rdname mcmc_layer
#' @keywords internal
layer_trace <- function(.truth = FALSE) {
  cur_scale <- scale_x_iterations()

  cur_labs <- ggplot2::labs(x = "Iteration", y = "Value", colour = "Chain")

  cur_geom_line <-
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data$.value,
        x = .data$.iter,
        colour = factor(.data$.chain)
      ),
      alpha = 0.55
    )

  cur_geom_hline <-
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$.truth),
      linetype = "dashed",
      alpha = 0.55
    )

  out <- list(cur_scale, cur_labs, cur_geom_line)
  return(if (.truth) append(out, cur_geom_hline) else out)
}


#' @rdname mcmc_layer
#' @keywords internal
layer_acf <- function() {
  cur_labs <- ggplot2::labs(y = "ACF", x = NULL, fill = "Chain")

  cur_geom_bar <-
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$acf, x = .data$lag, fill = factor(.data$.chain)),
      data = prepare_acf_data,
      stat = "identity",
      position = "dodge",
      width = 0.2,
      na.rm = TRUE
    )

  out <- list(cur_labs, cur_geom_bar)
  return(out)
}


#' @keywords internal
#' @param data data frame. MCMC output, requires `.chain` and `.term` columns.
#' @noRd
prepare_acf_data <- function(data) {
  data |>
    dplyr::group_by(.data$.term, .data$.chain) |>
    dplyr::reframe({
      .acf <- stats::acf(.data$.value, lag.max = NULL, plot = FALSE)
      as.data.frame(unclass(.acf)[c("acf", "lag")])
    })
}


#' Position Scale for MCMC Iterations
#'
#' Wrapper around [`scale_x_continuous`][ggplot2::scale_x_continuous()] with
#'   updated defaults for thousands of MCMC iterations.
#'
#' @param ... passed to `scale_x_continuous`
#' @param n integer. Number of desirable breaks.
#'
#' @keywords internal
scale_x_iterations <- function(..., n = 5) {
  defaults <-
    list(
      name = "Iterations",
      breaks = function(x) {
        br <- pretty(x, n = n)
        br[1] <- 0
        all_br_gt_1e3 <- all(log10(subset(br, br != 0)) >= 3)

        # if (all_br_gt_1e3) br[length(br)] <- 1e3 * floor(x[2] / 1e3)
        # if (!all_br_gt_1e3) br[length(br)] <- 1e2 * floor(x[2] / 1e2)

        return(unique(sort(br)))
      },
      labels = function(br) {
        all_br_gt_1e3 <- all(log10(subset(br, br != 0)) >= 3)
        sprintf(
          if (all_br_gt_1e3) "%.0fk" else "%.1fk",
          br / 1e3
        )
      }
    )

  args <- utils::modifyList(defaults, rlang::list2(...))

  return(do.call(ggplot2::scale_x_continuous, args))
}
