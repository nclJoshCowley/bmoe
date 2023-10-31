#' Complete JAGS Fit
#'
#' Fits a model in JAGS (compilation, warm-up and sampling) with a modern
#'   progress bar from `knitrProgressBar`.
#'
#' @param file,data,inits Passed to [rjags::jags.model()].
#'
#' @inheritParams bmoe
#' @inheritSection bmoe-args JAGS Controls
#'
#' @param varnames character. Variable names to be monitored via JAGS.
#'
#' @return list with
#'   - `data`: same as input.
#'   - `model`: object returned from [`jags.model`][rjags::jags.model()].
#'   - `output`: list of [`mcarray`][rjags::jags.samples()] objects.
#'   - `jags_n`: same as input.
#'
#' @keywords internal
complete_jags_fit <- function(file, data, inits, jags_n, varnames) {
  if (missing(inits)) inits <- NULL

  model <-
    rjags::jags.model(
      file = file,
      data = data,
      inits = inits,
      n.adapt = 0,
      n.chains = jags_n$n.chains,
      quiet = TRUE
    )

  initial_values <- model$state()

  message("\n", "Adaptation stage ...")

  jags_adapt <- rjags::adapt
  environment(jags_adapt) <- custom_update_jags(jags_n$n.adapt)

  ok <- jags_adapt(model, n.iter = jags_n$n.adapt, end.adaptation = FALSE)
  if (ok) {
    # .Call("adapt_off", model$ptr(), PACKAGE = "rjags")
    jags_adapt(model, n.iter = 0, end.adaptation = TRUE)
  } else {
    warning("Adaptation incomplete")
  }

  message("\n", "Update stage ...")

  update_jags <- custom_update_jags(jags_n$n.update, in_env = FALSE)
  update_jags(model, jags_n$n.update)

  message("\n", "Sampling stage ...")

  jags_samples <- rjags::jags.samples
  environment(jags_samples) <- custom_update_jags(jags_n$n.iter)

  output <- jags_samples(
    model = model,
    variable.names = varnames,
    n.iter = jags_n$n.iter,
    thin = jags_n$n.thin
  )

  output <- lapply(output, bmoe_array)

  return(list(
    data = data,
    model = model,
    output = output,
    jags_n = jags_n,
    inits = initial_values
  ))
}


#' @keywords internal
#' @noRd
custom_update_jags <- function(n.iter, steps = 100, in_env = TRUE) {
  update_jags <-
    function(object, n.iter, ...) {
      default_update.jags <- utils::getFromNamespace("update.jags", "rjags")

      .pb <- knitrProgressBar::progress_estimated(n = steps)

      sub.iter <- rep(floor(n.iter / steps), steps)
      sub.iter[1] <- sub.iter[1] + (n.iter %% steps)

      if (n.iter <= 100) sub.iter <- rep(1, n.iter)

      for (si in sub.iter) {
        default_update.jags(object, n.iter = si, progress.bar = "none", ...)
        knitrProgressBar::update_progress(.pb)
      }
    }

  if (isFALSE(in_env)) return(update_jags)

  rlang::child_env(.parent = rlang::ns_env("rjags"), update.jags = update_jags)
}
