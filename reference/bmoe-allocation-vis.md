# Visualise Posterior Group Membership, Ordered by Rank

Combination of allocation probabilities and quantile plots

## Usage

``` r
visualise_allocations_with_response_data(object, .chain, .rank_by)

visualise_allocation_data(object, .chain)

visualise_response_data(object, .plot_by, .rank_by)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- .chain:

  integer. Which chain indexes to use as posterior allocations.

- .rank_by:

  expression. Column to use in ordering (y-axis).

- .plot_by:

  expression. Column to be plotted (x-axis).

## Details

For consistency between each plot, ordering is based on a single chosen
response variable.

## Note

Both `.plot_by` and `.rank_by` can be censored (`<Surv>`).

## Posterior Allocation Plot

Histogram showing the corresponding proportions of each component
allocation per observation.

## NA

Histogram showing the corresponding proportions of each component
allocation per observation.
