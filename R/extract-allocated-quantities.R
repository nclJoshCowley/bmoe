#' Extract Allocated Quantities from Model Fit
#'
#' Allocated versions of generated quantities where allocation samples, `z`.
#'
#' @inheritParams bmoe-package
#' @param z [`bmoe_array`].
#'   MCMC samples used to integrate out components.
#'
#'   Defaults to [`extract_allocation_samples()`].
#'
#' @name bmoe-extract-allocated
#'
#' @returns All quantities returned as [`bmoe_array()`] of varying dimension.
#'
#' Note that `n_s_new` denotes the number of rows in `new_data` or the
#'   observed data when `new_data` is `NULL` valued.
NULL


#' @rdname bmoe-extract-allocated
#'
#' @returns * `extract_allocated_y_posterior_mean()`
#'     * `dim(.) = c(n_iters, n_chains, n_s_new, n_y)`.
#'
#' @export
extract_allocated_y_posterior_mean <- function(object, new_data, z = NULL) {
  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)
  n_y <- get_dims_from_bmoe_fit(object, "n_y")

  if (is.null(z)) z <- extract_allocation_samples(object, new_data)

  unallocated_y_mean <- extract_y_posterior_mean(object, new_data)

  pmap_bmoe_array(
    list(.y_mean = unallocated_y_mean, .z = z),
    function(.y_mean, .z) {
      out <- array(NA, dim = c(n_s_new, n_y))

      for (i in seq_len(n_s_new)) {
        for (yi in seq_len(n_y)) {
          out[i, yi] <- .y_mean[i, yi, .z[i]]
        }
      }

      return(out)
    },
    varname = "allocated_y_posterior_mean"
  )
}


#' @rdname bmoe-extract-allocated
#'
#' @returns * `extract_allocated_y_posterior_sd()`
#'     * Serves different purpose to [`extract_y_posterior_sd()`] since
#'       parameters are recycled per observation in `new_data`.
#'
#'     * `dim(.) = c(n_iters, n_chains, n_s_new, n_y)`.
#'
#' @export
extract_allocated_y_posterior_sd <- function(object, new_data, z = NULL) {
  new_data <- check_new_data(object, new_data)
  n_s_new <- nrow(new_data)
  n_y <- get_dims_from_bmoe_fit(object, "n_y")

  if (is.null(z)) z <- extract_allocation_samples(object, new_data)

  unallocated_y_sd <- extract_y_posterior_sd(object)

  pmap_bmoe_array(
    list(.y_sd = unallocated_y_sd, .z = z),
    function(.y_sd, .z) {
      out <- array(NA, dim = c(n_s_new, n_y))

      for (i in seq_len(n_s_new)) {
        for (yi in seq_len(n_y)) {
          out[i, yi] <- .y_sd[yi, .z[i]]
        }
      }

      return(out)
    },
    varname = "allocated_y_posterior_sd"
  )
}
