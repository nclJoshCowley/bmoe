# Simulate **Mixture of Experts Linear Regression**

Simulates from a VISP model and splits into training and test splits.

## Usage

``` r
example_simulate_bmoe(..., multiple_y = FALSE)

example_label_switching_bmoe()

simulate_bmoe(n_s, regr, wt, prec, n_loo, q_cens = NULL)
```

## Arguments

- ...:

  Passed to `simulate_bmoe`.

- multiple_y:

  logical. When `TRUE`, example defaults to multiple `y`.

- n_s:

  integer. Total number of simulated observations.

- regr:

  array. Regression coefficients.

- wt:

  matrix. Weighting coefficients.

- prec:

  numeric. Precision parameter per component.

- n_loo:

  integer. Number of observations to be moved into a test set.

- q_cens:

  numeric \[0, 1\]. Optional artificial left-censoring level.

## Value

List with class `bmoe_sim`.

## Details

- Since `dim(regr) = c(n_x + 1, n_y, n_k)`, the dimension of that input
  also asserts `n_x`, `n_y` and `n_k`.

- Component membership is simulated per observation, not per response
  variable. Hence, `dim(wt) = c(n_x + 1, n_k)`.

- The simulated `x` matrix is used for both regression and weighting.

- Only `n_s - n_loo` of the total observations will be kept in `$data`.

## Functions

- `example_label_switching_bmoe()`: Reproducible example of label
  switching example.
