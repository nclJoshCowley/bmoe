# Position Scale for MCMC Iterations

Wrapper around `scale_x_continuous` with updated defaults for thousands
of MCMC iterations.

## Usage

``` r
scale_x_iterations(..., n = 5)
```

## Arguments

- ...:

  passed to `scale_x_continuous`

- n:

  integer. Number of desirable breaks.
