#' Generated Quantities from Model Fit
#'
#' Offline calculations can
#'
#' @inheritParams bmoe-package
#'
#' @name bmoe-calculate
NULL


#' Retrieve or Simulate New Component Allocation Samples
#'
#' @rdname bmoe-calculate
#' @export
calculate_component_samples <- function(object, new_data) {
  if (is.null(new_data)) return(object$output$z)

  if (object$prior$k == 1) {
    expected_dims <-
      c(dim(object$output$z)[c("iteration", "chain")], nrow(new_data))

    return(bmoe::bmoe_array(array(1, dim = expected_dims), varname = "z"))
  }

  message("Drawing new allocation samples from relevant distribution")

  wt <- object$output$wt
  x_wt <- stats::model.matrix(object$formula$wt, data = new_data)

  all_labels <- seq_len(dim(wt)[4])

  probs <- pmap_bmoe_array(list(wt), function(.wt) softmax(x_wt %*% .wt))

  pmap_bmoe_array(
    list(probs = probs),
    function(probs) {
      apply(probs, 1, function(.p) sample(all_labels, size = 1, prob = .p))
    },
    varname = "z"
  )
}
