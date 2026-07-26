# Implementation of `component_allocation_visualisation`

Create `ggplot2` object on a chain-by-chain basis to help with
[`gc()`](https://rdrr.io/r/base/gc.html).

## Usage

``` r
impl_c_alloc_vis(x, labels)
```

## Arguments

- x:

  integer-valued matrix. Rows (columns) denote iterations (indices).

- labels:

  character. Labels to use in plot and `$data`.
