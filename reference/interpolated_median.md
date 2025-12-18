# Calculate the binned interpolated median

\#' @description **\[superseded\]**

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

## Details

This function is superceded by EPI's current preferred method for
interpolating medians,
[`averaged_median()`](https://economic.github.io/epidatatools/reference/averaged_median.md).

## Examples

``` r
interpolated_median(x = mtcars$mpg, bin_size = 0.50)
#> [1] 19
```
