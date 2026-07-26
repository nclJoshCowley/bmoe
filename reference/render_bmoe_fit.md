# Render Analysis Report

Render Quarto document and produce output files `.Rds` and `.html`.

## Usage

``` r
render_bmoe_fit(object, outfile)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- outfile:

  character. Name of output file(s); file extensions dropped.

## Value

Silently return the input `object` for use with pipes.
