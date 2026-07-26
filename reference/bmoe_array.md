# MCMC Array Type

MCMC array structure, `c(n_iters, n_chains, dim(.)[1], dim(.)[2], ...)`,
used in this package.

## Usage

``` r
bmoe_array(x, ...)

# S3 method for class 'array'
bmoe_array(x, ..., varname)

# S3 method for class 'mcarray'
bmoe_array(x, ...)

# S3 method for class 'bmoe_array'
print(x, fun = mean, ...)

# S3 method for class 'bmoe_array'
tidy(x, ..., .dimnames = NULL)
```

## Arguments

- x:

  object. To be coerced to different format or name used in generic.

- ...:

  Passed on for S3 methods; ignored in `bmoe_array`.

- varname:

  character. Variable name of the stored quantity.

- fun:

  function. Function to summarise when printing.

- .dimnames:

  character. Default behaviour only adds `.term <chr>`; supplying
  dimension names adds a column of indices for each dimension.

## Note

For general MCMC array use, consider packages `posterior` and `mcmcr` or
the original object types in `rjags` and `coda`.
