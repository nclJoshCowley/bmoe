
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
#>       y01    x01    x02     x03
#>     <dbl>  <dbl>  <dbl>   <dbl>
#>  1  0.567 -2.59  -0.711  0.784 
#>  2 -5.22  -0.940 -1.14  -1.74  
#>  3  5.62   0.573  0.285  0.0480
#>  4 -0.730 -1.27  -1.84  -0.119 
#>  5  3.97   0.777  0.120  0.769 
#>  6  9.59   1.72   1.02  -0.127 
#>  7 -4.38  -0.567 -0.826  0.186 
#>  8  3.94   0.210  0.510 -0.508 
#>  9  1.64   0.220 -0.349  1.64  
#> 10  2.85   0.509 -0.105  1.36  
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

More generally, this package provides a extended formula-data interface.

``` r
example_fit <-
  bmoe::bmoe(
    y01 ~ x01 + x02 + x03,
    data = example_sim$data,
    prior = example_prior
  )
```

This interface goes beyond the base R formula system as we can

- model multiple response variables as conditionally independent
  (conditional on component membership) use the `+` symbol in the LHS.

- allow two sets of predictors can be separated for regression purposes
  and component probability weighting purposes using `|`.

For example, `y01 + y02 ~ x01 + x02 + x03 | x03` implies two response
variables, `y01` and `y02`, to be regressed against the linear predictor
formed from `x01 + x02 + x03` according to some component probabilities
based on the linear predictor formed from `x03`.

### Reporting

Analysis reports can be generated from any fitted object and desired
filename.

``` r
bmoe::render_bmoe_fit(example_fit, "report-name")
```

## Future Work

Extend this `README` with more functionality including relabelling,
prediction and log likelihood.
