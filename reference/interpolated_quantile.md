# Calculate the binned interpolated quantile

**\[superseded\]**

This function is superceded by EPI's current preferred method for
interpolating quantiles,
[`averaged_quantile()`](https://economic.github.io/epidatatools/reference/averaged_quantile.md).

## Usage

``` r
interpolated_quantile(x, bin_size, probs = 0.5, w = NULL, na.rm = TRUE)
```

## Arguments

- x:

  numeric vector or an R object

- bin_size:

  size used for binning

- probs:

  numeric; percentile with value `[0,1]`

- w:

  numeric vector of weights the same length as x giving the weights to
  use for elements of x

- na.rm:

  logical; if true, any NA or NaN's are removed from x before
  computation

## Value

a numeric vector

## Examples

``` r
interpolated_quantile(x = mtcars$mpg, bin_size = 0.50, probs = c(0.25, 0.5, 0.75))
#> [1] 15.25 19.00 22.50
```
