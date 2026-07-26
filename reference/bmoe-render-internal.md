# Functions used in Analysis Report

Functions not to be used anywhere other than `bmoe-analysis.qmd`.

## Usage

``` r
.set_bmoe_render_options(object, is_child, fig.asp)
```

## Arguments

- object:

  Object that inherits from
  [`bmoe_fit`](https://ncljoshcowley.github.io/bmoe/reference/bmoe.md).

- is_child:

  logical. Some options are only set when this is `FALSE`.

- fig.asp:

  numeric. Default aspect ratio per **panel**, not per plot.
