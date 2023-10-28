
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Purpose

<!-- badges: start -->
<!-- badges: end -->

This repository contains an R package with a focus on Mixture of Experts
(MoE) applied to Bayesian linear and censored regression.

## Installation

You can install the development version of `bmoe` from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("nclJoshCowley/bmoe", auth_token = ...)
```

## Model Description

TODO: Include model description here.

## Worked Example

### Simulation

We can simulate data from these models using `bmoe::simulate_bmoe()`.

Alternatively, one can use the example wrapper to utilise default
arguments.

``` r
example_sim <- bmoe::example_simulate_bmoe()

example_sim$data
#> # A tibble: 180 × 4
#>        y01     x01     x02     x03
#>      <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -0.0653 -0.733   0.0144  0.883 
#>  2  2.89   -1.32    0.368  -0.106 
#>  3  0.313   0.385   1.28   -0.276 
#>  4 -3.18   -0.0525 -0.154   0.0295
#>  5  0.213   1.57   -1.45   -2.30  
#>  6 -1.52   -1.13   -1.38    0.0984
#>  7  3.87    0.477  -0.0988  1.05  
#>  8 -1.90   -0.186  -0.560   1.02  
#>  9  2.85   -0.221  -0.822   0.785 
#> 10  3.75   -0.164  -0.115   0.945 
#> # ℹ 170 more rows
```

### Fitting

We require prior hyperparameters for all models; the default is a vague
prior for each parameter but the number of components ($K$) is assumed
known and must be set by the user.

``` r
example_prior <- bmoe::bmoe_prior(k = 3)

example_prior
#> $k
#> [1] 3
#> 
#> $regr_prec
#> [1] 0.1
#> 
#> $wt_prec
#> [1] 1
#> 
#> $prec_shape
#> [1] 2
#> 
#> $prec_rate
#> [1] 1
```

For simulation study results, one can pass a simulation directly.

``` r
bmoe::bmoe(example_sim, prior = example_prior)
```

More generally, this package provides a formula-data interface.

``` r
example_fit <-
  bmoe::bmoe(
    y01 ~ x01 + x02 + x03,
    data = example_sim$data,
    prior = example_prior
  )
```

### Reporting

Analysis reports can be generated from any fitted object and desired
filename.

``` r
bmoe::render_bmoe_fit(example_fit, "report-name")
```

## Future Work

Extend this `README` with more functionality including relabelling,
prediction and log likelihood.
