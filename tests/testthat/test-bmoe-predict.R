test_that("Similar results for combined and individual predictions (K = 1)", {
  fit <-
    example_bmoe_fit(
      multiple_y = TRUE,
      n_loo = 5,
      prior = bmoe::bmoe_prior(k = 1),
      jags_n = bmoe::bmoe_jags_n(n.update = 0, n.iter = 2e3, n.chains = 1)
    )

  fit$output$prec <- array(1e+64, dim = dim(fit$output$prec))

  p_combined <-
    stats::predict(fit, new_data = fit$new_data, type = "response") |>
    dplyr::select(".pred_y01") |>
    tidyr::unnest(cols = ".pred_y01")

  p_separate <-
    dplyr::bind_rows(
      lapply(seq_len(nrow(fit$new_data)), function(.i) {
        stats::predict(fit, new_data = fit$new_data[.i, ], type = "response")
      })
    ) |>
    dplyr::select(".pred_y01") |>
    tidyr::unnest(cols = ".pred_y01")

  expect_equal(p_combined, p_separate)
})
