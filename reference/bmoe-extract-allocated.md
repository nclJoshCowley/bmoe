# Extract Allocated Quantities from Model Fit

Allocated versions of generated quantities where allocation samples,
`z`.

## Usage

``` r
extract_allocated_y_posterior_mean(object, new_data, z = NULL)

extract_allocated_y_posterior_sd(object, new_data, z = NULL)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- new_data:

  data frame. Similar structure to original `data` argument.

  Setting `new_data = NULL` signals for the observed data to be used.

- z:

  [`bmoe_array`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md).
  MCMC samples used to integrate out components.

  Defaults to
  [`extract_allocation_samples()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe-extract-quantities.md).

## Value

All quantities returned as
[`bmoe_array()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md)
of varying dimension.

Note that `n_s_new` denotes the number of rows in `new_data` or the
observed data when `new_data` is `NULL` valued.

- `extract_allocated_y_posterior_mean()`

  - `dim(.) = c(n_iters, n_chains, n_s_new, n_y)`.

&nbsp;

- `extract_allocated_y_posterior_sd()`

  - Serves different purpose to
    [`extract_y_posterior_sd()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe-extract-quantities.md)
    since parameters are recycled per observation in `new_data`.

  - `dim(.) = c(n_iters, n_chains, n_s_new, n_y)`.
