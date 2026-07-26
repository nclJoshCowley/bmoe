# Validate Permutations

Validate or create described permutation structure.

## Usage

``` r
validate_permutations(x, n_chains, n_iters)

# S3 method for class 'list'
validate_permutations(x, n_chains, n_iters)

# S3 method for class 'matrix'
validate_permutations(x, n_chains, n_iters)

# S3 method for class 'numeric'
validate_permutations(x, n_chains, n_iters)
```

## Arguments

- x:

  list; matrix; vector. Object to be coerced into permutations.

- n_chains, n_iters:

  integer. Dimensions to check against or be assumed.

## Permutations

Permutations are stored in a list where \\i^{th}\\ elements corresponds
to the \\i^{th}\\ chain.

Each element is a matrix with one permutation per row for each
iteration. Note that

- A vector can be supplied to apply the same scheme to all MCMC output.

- A matrix can be supplied to apply the same relabelling to all chains.

- `NULL` can be supplied to use
  [`boys_henderson_2002()`](https://ncljoshcowley.github.io/bmoe/reference/boys_henderson_2002.md).

A permutation of `c(3, 1, 2)` denotes relabelling of

- `1 -> 3`

- `2 -> 1`

- `3 -> 2`
