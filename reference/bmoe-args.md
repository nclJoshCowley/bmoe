# Bayesian MoE (`bmoe`) Arguments

Helper functions to assist in supplying arguments to
[`bmoe()`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

## Usage

``` r
bmoe_prior(k, ...)

bmoe_jags_n(...)

bmoe_inits(seed_base, .RNG.name = "base::Wichmann-Hill", ...)
```

## Arguments

- k:

  integer. Assumed number of components.

- ...:

  Overwrite default values.

- seed_base:

  numeric. Seed are set equal to `seed_base` multiplied by chain index.

- .RNG.name:

  character. JAGS RNG method.

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

## JAGS Initial Values

Optional chain starting values, as described in
[`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html).
