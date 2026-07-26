# Likelihood of Left-Censored Normally Distributed Vector

Likelihood of Left-Censored Normally Distributed Vector

## Usage

``` r
d_lcens_norm(x, mean, sd, log = FALSE)
```

## Arguments

- x:

  object. Expected to be left-censored `Surv`.

- mean, sd, log:

  Passed to [`stats::dnorm`](https://rdrr.io/r/stats/Normal.html) and
  [`stats::pnorm`](https://rdrr.io/r/stats/Normal.html).

## Note

Passing non-`Surv` objects lead to `stats::dnorm(...)`, not an error.
