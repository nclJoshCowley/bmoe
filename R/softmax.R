#' `Softmax` Function
#'
#' Smoothed version of "argmax"; also known as the inverse multinomial logit.
#'
#' @param x objects of vector or matrix.
#' @param margin integer. Dimension to apply the function over,
#'   defaults to 1 (rows).
#' @inheritParams rlang::args_dots_empty
#'
#' @name softmax
NULL


#' @rdname softmax
#' @export
softmax <- function(x, ...) UseMethod("softmax")


#' @rdname softmax
#' @export
softmax.default <- function(x, ...) {
  stop(sprintf("softmax method for %s not supported.", toString(class(x))))
}


#' @rdname softmax
#' @export
softmax.numeric <- function(x, ...) {
  rlang::check_dots_empty()
  exp_x <- exp(x)
  if (any(is.infinite(exp_x))) stop("exp(x) not finite in softmax calculation.")

  return(exp_x / sum(exp_x))
}


#' @rdname softmax
#' @export
softmax.matrix <- function(x, margin = 1, ...) {
  rlang::check_dots_empty()
  exp_x <- exp(x)
  if (any(is.infinite(exp_x))) stop("exp(x) not finite in softmax calculation.")

  return(sweep(exp_x, margin, apply(exp_x, margin, sum), `/`))
}
