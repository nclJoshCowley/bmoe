# Extract Log Likelihood from Model Fit

Offline calculations of log likelihood(s) from a `bmoe` model fit.

## Usage

``` r
extract_log_lik(object, new_data)

extract_pointwise_log_lik(object, new_data)

extract_componentwise_pointwise_log_lik(object, new_data)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- new_data:

  data frame. Similar structure to original `data` argument.

  Setting `new_data = NULL` signals for the observed data to be used.

## Value

- `extract_log_lik()`

  - Scalar log likelihood for each MCMC iteration.

  - `dim(.) = c(n_iters, n_chains, 1)`.

&nbsp;

- `extract_pointwise_log_lik()`

  - Pointwise likelihood is sum of each component contribution, weighted
    by the allocation probabilities.

  - `dim(.) = c(n_iters, n_chains, n_s_new, n_y, n_k)`.

&nbsp;

- `extract_componentwise_pointwise_log_lik()`

  - Evaluated at each component (expert) and each observation in `y`.

  - `dim(.) = c(n_iters, n_chains, n_s_new, n_y, n_k)`.

## See also

The `loo` package for using these quantities in model comparison.
