#' Validate Permutations
#'
#' Validate or create described permutation structure.
#'
#' @param x list; matrix; vector. Object to be coerced into permutations.
#' @param n_chains,n_iters integer. Dimensions to check against or be assumed.
#'
#' @inheritSection apply_relabelling Permutations
#'
#' @keywords internal
validate_permutations <- function(x, n_chains, n_iters) {
  UseMethod("validate_permutations")
}


#' @rdname validate_permutations
#' @keywords internal
validate_permutations.list <- function(x, n_chains, n_iters) {
  if (length(x) != n_chains) {
    stop("Permutations should have one element per chain")
  }

  nrows <- vapply(x, nrow, integer(1))

  if (any(nrows != n_iters)) {
    stop(sprintf(
      "Expected %i rows for each element but got c(%s)",
      n_iters, toString(nrows)
    ))
  }

  return(x)
}


registerS3method("validate_permutations", "list", validate_permutations.list)


#' @rdname validate_permutations
#' @keywords internal
validate_permutations.matrix <- function(x, n_chains, n_iters) {
  list_x <- rep(list(x), n_chains)
  validate_permutations.list(list_x, n_chains, n_iters)
}


registerS3method("validate_permutations", "matrix", validate_permutations.matrix)


#' @rdname validate_permutations
#' @keywords internal
validate_permutations.numeric <- function(x, n_chains, n_iters) {
  stopifnot(is.null(dim(x)))
  matrix_x <- matrix(x, nrow = n_iters, ncol = length(x), byrow = TRUE)
  validate_permutations.matrix(matrix_x, n_chains, n_iters)
}


registerS3method("validate_permutations", "numeric", validate_permutations.numeric)
