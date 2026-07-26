# Convert `bmoe_array` to Prediction Table

Creates a list column per response (or class) where each element is a
draws tibble containing `.iter`, `.chain` and `.value` for a single
parameter.

## Usage

``` r
bmoe_array_to_pred_tb(x, .key)
```

## Arguments

- x:

  [`bmoe_array`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md)
  object.

- .key:

  character. Names to use in output, length should be `dim(x)[4]`.
