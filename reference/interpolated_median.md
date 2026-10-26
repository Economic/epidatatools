# Calculate the binned interpolated median

**\[deprecated\]**

This function was deprecated in epidatatools 2.0.0. Use
[`averaged_median()`](https://economic.github.io/epidatatools/reference/averaged_median.md),
EPI's preferred method for interpolating medians, instead. It uses a
different method, so its results differ.

## Usage

``` r
interpolated_median(x, bin_size, w = NULL, na.rm = TRUE)
```

## Arguments

- x:

  numeric vector or an R object

- bin_size:

  size used for binning

- w:

  numeric vector of weights the same length as x giving the weights to
  use for elements of x

- na.rm:

  logical; if true, any NA or NaN's are removed from x before
  computation

## Value

numeric vector

## Examples

``` r
interpolated_median(x = mtcars$mpg, bin_size = 0.50)
#> Warning: `interpolated_median()` was deprecated in epidatatools 2.0.0.
#> ℹ Please use `averaged_median()` instead.
#> [1] 19
```
