# Subset MCMC Output

Subset the MCMC output and update details within a
[`bmoe()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md) fit.

## Usage

``` r
subset_bmoe(object, iters = NULL, chains = NULL)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- iters:

  integer. Vector of indices to keep, default includes all.

- chains:

  integer. Vector of chains to keep, default includes all.
