object <-
  bmoe(
    example_simulate_bmoe(n_s = 20, n_loo = 5, multiple_y = TRUE, q_cens = 0.1),
    prior = bmoe_prior(k = 3),
    jags_n = list(n.update = 0, n.adapt = 200, n.iter = 10000)
  )

# Prediction?
# Ordered rank-allocation plot?
