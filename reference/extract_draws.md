# Extract Posterior Samples from Bayesian Mixture of Experts

Extract Posterior Samples from Bayesian Mixture of Experts

## Usage

``` r
extract_draws(object, varname, ...)

# S3 method for class 'bmoe_fit'
extract_draws(object, varname, ...)

# S3 method for class 'bmoe_simstudy'
extract_draws(object, varname, ...)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- varname:

  character. Variable name defined in the model.

- ...:

  These dots are for future extensions and must be empty.

## Simulation Study

For models fit to a `bmoe_sim` object, a `$.truth` column may also be
added.

This functionality is conditional on the assumed \\K\\ in `prior` being
the same as the \\K\\ used to simulate the data.
