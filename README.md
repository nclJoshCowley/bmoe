
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

## Purpose

This repository contains an R package with a focus on Mixture of Experts
(MoE) applied to Bayesian linear and censored regression.

Originally used in a yet to be published thesis, **work is ongoing** to
make this publicly accessible package easier to use.

Tasks remaining include

- Adding `pkgdown` then improving documentation (`README`, vignettes,
  etc.),

- Possibly migrating to Stan to avoid the JAGS ‘ones trick’,

- Removing unnecessary code and dependencies,

- Submitting to CRAN for wider use.

## Installation

You can install the development version of `bmoe` from
[GitHub](https://github.com/nclJoshCowley/bmoe) with:

``` r
remotes::install_github("nclJoshCowley/bmoe")
```

## Model Description

We present the Mixture of Experts model as a finite mixture model of $K$
parametric linear regressions, where the concomitant weighting
parameters also depend on some predictors.

$$
    f(\boldsymbol{y_i} | \boldsymbol{x}_i, \boldsymbol{\omega}, \boldsymbol{\theta})
        = \sum_{k=1}^K
            \eta_k(\boldsymbol{x}_i | \boldsymbol{\omega}_k)
            f_k(\boldsymbol{y_i} | \boldsymbol{x}_i, \boldsymbol{\theta}_k)
$$

Each components’ distribution is currently limited to (conditionally
independent) multiple linear regressions where each response variable
can potentially be left-censored.

<!-- TODO: Article to explain what this model is in semi-technical detail -->

## Worked Example

### Simulation

We can simulate data from these models using `bmoe::simulate_bmoe()`.

Alternatively, one can use the example wrapper to utilise default
arguments.

``` r
example_sim <- bmoe::example_simulate_bmoe()

example_sim$data
#> # A tibble: 180 × 4
#>       y01     x01     x02     x03
#>     <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -4.03  -1.07   -0.0269 -1.03  
#>  2  1.26  -1.36   -1.34    0.901 
#>  3 -4.38   1.37   -2.21   -0.277 
#>  4 -0.490 -0.463   0.0581  0.0647
#>  5 -0.211  0.0454 -0.446  -0.406 
#>  6  7.45   0.456   0.956   0.927 
#>  7  4.92   1.03    1.78    2.56  
#>  8 -0.140 -0.312  -0.428   0.222 
#>  9 -0.162  0.195   0.0182 -0.836 
#> 10  2.69   2.47   -1.46   -0.473 
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
file name.

``` r
bmoe::render_bmoe_fit(example_fit, "report-name")
```
