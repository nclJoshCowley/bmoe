object <-
  bmoe(
    example_simulate_bmoe(n_s = 30, n_loo = 10, multiple_y = TRUE, q_cens = 0.1),
    prior = bmoe_prior(k = 3),
    jags_n = list(n.update = 0, n.iter = 2000)
  )


# data <- object$data

# Prediction?
# Label switching?
# Ordered rank-allocation plot?
