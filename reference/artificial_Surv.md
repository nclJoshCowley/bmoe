# Artificial Censoring

Convert numeric vector to a
[`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html) object by
censoring at chosen quantiles.

## Usage

``` r
artificial_Surv(x, q_cens)
```

## Arguments

- x:

  vector. Uncensored numerical data.

- q_cens:

  numeric \[0, 1\]. Optional artificial left-censoring level.

## Value

Surv object.
