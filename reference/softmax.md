# `Softmax` Function

Smoothed version of "argmax"; also known as the inverse multinomial
logit.

## Usage

``` r
softmax(x, ...)

# Default S3 method
softmax(x, ...)

# S3 method for class 'numeric'
softmax(x, ...)

# S3 method for class 'matrix'
softmax(x, margin = 1, ...)
```

## Arguments

- x:

  objects of vector or matrix.

- ...:

  These dots are for future extensions and must be empty.

- margin:

  integer. Dimension to apply the function over, defaults to 1 (rows).
