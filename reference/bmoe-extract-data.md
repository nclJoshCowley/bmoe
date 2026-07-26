# Extract Data from Model Fit

Getter functions for the data used in a `bmoe` model fit.

## Usage

``` r
extract_x_regr(object, new_data)

extract_x_wt(object, new_data)

extract_y(object, new_data)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- new_data:

  data frame. Similar structure to original `data` argument.

  Setting `new_data = NULL` signals for the observed data to be used.

## Value

`extract_y()` returns a list of vectors, one per `y` variable.
