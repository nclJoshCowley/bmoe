# Apply function to each MCMC Draw

Maps a function over each MCMC iteration and chain of a
[`bmoe_array`](https://ncljoshcowley.github.io/bmoe/reference/bmoe_array.md).

## Usage

``` r
pmap_bmoe_array(.l, .f, ..., varname = NULL)
```

## Arguments

- .l:

  list of `bmoe_array`. Only MCMC inputs should be placed here.

- .f:

  function. To be applied over each draws.

- ...:

  Deterministic arguments. Same value are passed to `.f` each time.

- varname:

  character. Variable name of the stored quantity.

## Value

A newly generated MCMC quantity, stored as a `bmoe_array`.
