# Parse MoE Formula

Internal method to allow multiple response and multiple RHS within a
[`formula`](https://rdrr.io/r/stats/formula.html).

## Usage

``` r
parse_bmoe_formula(object)
```

## Arguments

- object:

  formula. User supplied model description.

## Extended Formula

For more information on extending `formula`, see the `Formula` package
but be warned about <https://github.com/rstudio/rstudio/issues/12409>.

We allow the user to jointly model multiple response variables using
`+`:

- `y01 + y02 ~ x01 + x02`

- `y01 + y02 ~ .`

The RHS can also be split into two parts using `|` where the former is
used for regression and the latter is used for weighting. A formula with
only one part asserts a single design matrix for both.

- `y01 ~ x01 + x02 | x03`

- `y01 + y02 ~ . | x01`
