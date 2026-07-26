# List of MoE Analysis Plot Objects

Creates a nested list of `ggplot2` objects with partitioned data,
requires the user to add
[layers](https://ncljoshcowley.github.io/bmoe/reference/mcmc_layer.md).

## Usage

``` r
# S3 method for class 'bmoe_fit'
autoplot(object, varname, type = "none", ..., new_data)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- varname:

  character. Variable name defined in the model.

- type:

  choice. Name of the supported plot type including

  - `"none"`, plots are to be empty until the user adds layers.

  - `"density"`, useful distribution visualisation.

  - `"trace"`, useful for verifying chain convergence.

  - `"acf"`, autocorrelation of the MCMC samples at discrete lags.
    Alternatively one can pass a **list** of custom layers to be added.

- ...:

  Extra arguments silently ignored.

- new_data:

  Passed to
  [`extract_log_lik()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe-log-lik.md);
  ignored otherwise.
