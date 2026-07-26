# Print Tabsets (via [`knitr::knit_print`](https://rdrr.io/pkg/knitr/man/knit_print.html))

Converts a list to a Quarto / RMD tabset using the names as tab
headings.

## Usage

``` r
printer_tabset(x, options, ...)
```

## Arguments

- x:

  list. Each element is passed to `knit_print` within tabs.

- options, ...:

  unused arguments required for `knit_print`.

## References

<https://github.com/nclJoshCowley/jcutils/blob/master/R/knitr-printers.R>
