# Extract Single Draw

Extract a single draw for specified iteration and chain.

## Usage

``` r
extract_single_draw(x, iter, chain)
```

## Arguments

- x:

  [`bmoe_array`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md)
  object.

- iter, chain:

  integer. MCMC indexes to extract from.

## Note

Only the MCMC dimensions are
[dropped](https://rdrr.io/r/base/drop.html).
