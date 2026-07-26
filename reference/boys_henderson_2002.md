# Relabelling Algorithm, Boys & Henderson (2002)

Relabelling algorithm detailed in paper "On determining the order of
Markov dependence of an observed process" (Boys & Henderson, 2002).

## Usage

``` r
boys_henderson_2002(z, start = 2, z_max = max(z))
```

## Arguments

- z:

  `bmoe_array`. Component allocation output from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- start:

  integer. Required starting point for this algorithm.

- z_max:

  integer. Maximum possible value of `z`.

## Value

See **Permutations** section in
[`apply_relabelling()`](https://ncljoshcowley.github.io/bmoe/reference/apply_relabelling.md).

## Details

Read paper for full details; aim is to minimise 'disagreement',

\\ D = - \sum\_{i = 1}^n \mathbb{I} \left( \nu_m(z_i^{(m)}) =
\hat{z}\_i^{(m - 1)} \right) \\

where

- \\\nu_m\\ is a candidate permutation to be chosen;

- \\\hat{z}\\ is the marginal posterior mode of all MCMC iterations up
  to that point.

- relabelling is applied per MCMC iterations, \\m = m_0, \dots, M\\
  where the user defines \\m_0\\ using the `start` argument.

## References

Boys, R. J. & Henderson, D. A. 2002 On determining the order of markov
dependence of an observed process governed by a hidden markov model.
Scientific Programming 10 (3), 241–251.
