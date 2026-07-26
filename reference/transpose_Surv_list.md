# Splits a list of `Surv` objects into JAGS data

Convert list of `Surv` objects to two matrices of identical dimension.

## Usage

``` r
transpose_Surv_list(y)
```

## Arguments

- y:

  list. Elements not inheriting `Surv` assumed to be uncensored data.

## Value

List with `y` matrix and `is_nd` matrix.
