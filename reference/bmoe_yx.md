# Mixture of Experts Implementation

Implementation of Mixture of Experts models.

## Usage

``` r
bmoe_yx(y_list, x_regr, x_wt, prior, jags_n, inits = NULL)
```

## Arguments

- y_list:

  list. Response data per element; length is 1 when univariate.

- x_regr, x_wt:

  matrix. Regression and weighting matrices.

- prior:

  named list. See **Prior** section.

- jags_n:

  named list. See **JAGS Controls** section.

- inits:

  list. Passed to
  [`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html).
