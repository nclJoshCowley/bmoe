# Extract Generated Quantities from Model Fit

Offline calculations for model artefacts from a `bmoe` model fit.

## Usage

``` r
extract_y_posterior_mean(object, new_data)

extract_y_posterior_sd(object)

extract_allocation_probs(object, new_data)

extract_allocation_samples(object, new_data)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- new_data:

  data frame. Similar structure to original `data` argument.

  Setting `new_data = NULL` signals for the observed data to be used.

## Value

All quantities returned as
[`bmoe_array()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md)
of varying dimension.

Note that `n_s_new` denotes the number of rows in `new_data` or the
observed data when `new_data` is `NULL` valued.

- `extract_y_posterior_mean()`

  - `dim(.) = c(n_iters, n_chains, n_s_new, n_y, n_k)`.

&nbsp;

- `extract_y_posterior_sd()`

  - `dim(.) = c(n_iters, n_chains, n_y, n_k)`.

&nbsp;

- `extract_allocation_probs()`

  - `dim(.) = c(n_iters, n_chains, n_s_new, n_k)`.

&nbsp;

- `extract_allocation_samples()`

  - `dim(.) = c(n_iters, n_chains, n_s_new)`.

## See also

[Allocated
versions](https://ncljoshcowley.github.io/bmoe/reference/bmoe-extract-allocated.md)
of these functions.
