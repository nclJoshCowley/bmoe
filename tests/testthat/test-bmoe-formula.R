expect_equal_formulas <- function(object, expected, ...) {
  testthat::expect_equal(object, expected, ..., ignore_attr = TRUE)
}


test_that("Empty LHS carries forward", {
  output <- parse_bmoe_formula(~ x01 + x02)

  expect_equal_formulas(
    parse_bmoe_formula(~ x01 + x02 | x03)$regr,
    list(~ x01 + x02)
  )

  expect_equal_formulas(
    parse_bmoe_formula(~ x01 + x02 | x03)$wt,
    ~ x03
  )
})


test_that("Formulas with multi-response split 'regr' into list", {
  expect_equal_formulas(
    parse_bmoe_formula(y01 + y02 ~ .)$regr,
    list(y01 ~ ., y02 ~ .)
  )

  expect_equal_formulas(
    parse_bmoe_formula(y01 + y02 ~ x01)$regr,
    list(y01 ~ x01, y02 ~ x01)
  )

  expect_equal_formulas(
    parse_bmoe_formula(y01 + y02 ~ x01)$wt,
    ~ x01
  )
})


test_that("Formulas with multiple RHS parts split into 'regr' and 'wt'", {
  # Single response
  expect_equal_formulas(
    parse_bmoe_formula(y01 ~ . | x03)$regr,
    list(y01 ~ .)
  )

  expect_equal_formulas(
    parse_bmoe_formula(y01 ~ . | x03)$wt,
    ~ x03
  )

  # Multiple response
  expect_equal_formulas(
    parse_bmoe_formula(y01 + y02 ~ x01 + x02 | x03)$regr,
    list(y01 ~ x01 + x02, y02 ~ x01 + x02)
  )

  expect_equal_formulas(
    parse_bmoe_formula(y01 + y02 ~ x01 + x02 | x03)$wt,
    ~ x03
  )
})
