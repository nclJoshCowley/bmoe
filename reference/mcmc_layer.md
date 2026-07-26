# Get `ggplot2` Layer(s) for Plotting MCMC Output.

Get `ggplot2` Layer(s) for Plotting MCMC Output.

## Usage

``` r
mcmc_layer(type, .truth = FALSE)

layer_density(.truth = FALSE)

layer_trace(.truth = FALSE)

layer_acf()
```

## Arguments

- type:

  choice. Name of the supported plot type including

  - `"none"`, plots are to be empty until the user adds layers.

  - `"density"`, useful distribution visualisation.

  - `"trace"`, useful for verifying chain convergence.

  - `"acf"`, autocorrelation of the MCMC samples at discrete lags.
    Alternatively one can pass a **list** of custom layers to be added.
