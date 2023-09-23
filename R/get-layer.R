#' Get `ggplot2` Layer(s) for Plotting Functionality
#'
#' @param type choice. Name of the supported plot type including
#' * `"none"`, plots are to be empty until the user adds layers.
#' * `"density"`, useful distribution visualisation.
#' * `"trace"`, useful for verifying chain convergence.
#' * `"acf"`, autocorrelation of the MCMC samples at discrete lags.
#'
#' @name get_layer
NULL


#' @rdname get_layer
#' @keywords internal
get_layer <- function(type = "none") {
  switch(
    match.arg(type, c("none", "density", "trace", "acf")),
    none = list(),
    density = layer_density(),
    trace = layer_trace(),
    acf = layer_acf()
  )
}


#' @rdname get_layer
#' @keywords internal
get_truth_layer <- function(type) {
  type <- match.arg(type, c("density", "trace", "acf"))

  if (type == "density") {
    return(ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data$.truth),
      linetype = "dashed",
      alpha = 0.55
    ))
  }

  if (type == "trace") {
    return(ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$.truth),
      linetype = "dashed",
      alpha = 0.55
    ))
  }

  return(NULL)
}


#' @rdname get_layer
#' @keywords internal
layer_density <- function() {
  list(
    ggplot2::geom_density(
      mapping = ggplot2::aes(x = .data$.value, fill = factor(.data$.chain)),
      adjust = 1.5,
      alpha = 0.1,
    ),

    ggplot2::labs(y = NULL, x = NULL, fill = "Chain")
  )
}


#' @rdname get_layer
#' @keywords internal
layer_trace <- function() {
  list(
    geom = ggplot2::geom_line(
      mapping = ggplot2::aes(
        y = .data$.value,
        x = .data$.iter,
        colour = factor(.data$.chain)
      )
    ),

    labs = ggplot2::labs(x = "Iteration", y = "Value", colour = "Chain"),

    scales = scale_x_iterations()
  )
}


#' @rdname get_layer
#' @keywords internal
layer_acf <- function() {
  list(
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = .data$acf, x = .data$lag, fill = factor(.data$.chain)
      ),
      data = prepare_acf_data,
      stat = "identity",
      position = "dodge",
      width = 0.2
    ),

    ggplot2::labs(y = "ACF", x = NULL, fill = "Chain")
  )
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
