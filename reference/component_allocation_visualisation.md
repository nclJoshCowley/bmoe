# Visualise Component Allocation

Show a raster plot where

- each row is an observation;

- each column is an individual MCMC iteration;

- each tile is coloured according to component membership.

## Usage

``` r
component_allocation_visualisation(object, chain)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- chain:

  integer. Specified chain(s), multiple chains forces list output.
