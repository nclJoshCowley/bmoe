# Apply Relabelling Scheme to [`bmoe`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md)

Allows swapping of component labels, per MCMC iteration, to mitigate
label switching.

## Usage

``` r
apply_relabelling(object, permutations = NULL)

# S3 method for class 'bmoe_fit'
apply_relabelling(object, permutations = NULL)

table_permutations(permutations)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- permutations:

  list. See **Permutations** section.

## Value

[`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md)
object with switched labels.

## Functions

- `table_permutations()`: Calculate summary of a `permutations` object.

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
