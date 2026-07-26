# Prediction for Bayesian Mixture of Expert Fits

Both response and component membership can be generated.

## Usage

``` r
# S3 method for class 'bmoe_fit'
predict(object, ..., new_data, type, summarise = TRUE)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- ...:

  Extra arguments ignored.

- new_data:

  data frame. Similar structure to original `data` argument.

  Setting `new_data = NULL` signals for the observed data to be used.

- type:

  choice. Determines output type.

- summarise:

  logical. When `TRUE`, any list column output is summarised.

## Value

- `type = "response"`. Tibble output with a list column per response.

- `type = "class"`. Component membership, `z`, is returned as a list
  column.

- `type = "raw"`. A
  [`bmoe_array`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md)
  MCMC object is returned.
