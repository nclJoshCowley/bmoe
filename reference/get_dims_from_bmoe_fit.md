# Get Data and Parameter Dimensions

Helper to access names from data (`y`, `x`, `k`) to be displayed to
users.

## Usage

``` r
get_dims_from_bmoe_fit(object, id)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- id:

  choice. One of

  - `n_iters`: Number of iterations.

  - `n_chains`: Number of chains.

  - `n_s`: Number of observations.

  - `n_y`: Number of response variables.

  - `n_k`: Assumed number of components.

  - `p_regr`: Number of regression predictors (including intercept).

  - `p_wt`: Number of weighting predictors (including intercept).
