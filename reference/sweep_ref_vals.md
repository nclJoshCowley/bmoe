# Sweep Out Reference Values

Translates matrix to satisfy corner constraint, say `x[, ref] = 0`.

## Usage

``` r
sweep_ref_vals(x, ref = 1)
```

## Arguments

- x:

  matrix. Matrix to be translated.

- ref:

  integer. Column index to be used as reference.

## Value

Matrix with elements defined as `out[i, j] = x[i, j] - x[i, ref]`.
