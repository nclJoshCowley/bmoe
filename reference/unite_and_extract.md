# Unite Columns into String

Unite Columns into String

## Usage

``` r
unite_and_extract(data, sep = ",")
```

## Arguments

- data:

  data frame. Contains columns to be joined.

- sep:

  character. Passed to `paste`.

## Value

character vector, `paste(df[, 1], ..., df[, N], sep = sep)`.

## See also

[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html).
