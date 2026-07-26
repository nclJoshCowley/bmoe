# Bayesian Mixture of Experts Linear Regression.

Fits the model in JAGS.

## Usage

``` r
bmoe(object, ..., prior, jags_n = bmoe_jags_n(), inits = NULL)

# S3 method for class 'formula'
bmoe(object, data, ..., prior, jags_n = bmoe_jags_n(), inits = NULL)

# S3 method for class 'bmoe_sim'
bmoe(object, ..., prior, jags_n = bmoe_jags_n(), inits = NULL)
```

## Arguments

- object:

  object. A formula or simulated object.

- ...:

  These dots are for future extensions and must be empty.

- prior:

  named list. See **Prior** section.

- jags_n:

  named list. See **JAGS Controls** section.

- inits:

  list. Passed to
  [`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html).

- data:

  data frame. To be used in modelling.

## Prior

Hyper-parameters that must be passed to the `prior` argument are in
bold.

- `k`, number of components, assumed known.

- `regr` is element-wise IID Normal with 0 mean and **regr_prec**
  precision.

- `wt` is element-wise IID Normal with 0 mean and **wt_prec** precision.

- `prec` is element-wise IID Gamma with **prec_shape** and
  **prec_rate**.

## JAGS Controls

- `n.adapt` controls number of discarded samples in adaptation stage.

- `n.update` controls number of discarded samples in warm-up stage.

- `n.iter` controls how many samples are saved.

- `n.thin` controls thinning, where only every \\n^th\\ sample is kept.

- `n.chains` controls number of chains.

## See also

[`example_bmoe_fit()`](https://ncljoshcowley.github.io/bmoe/reference/example_bmoe_fit.md).
