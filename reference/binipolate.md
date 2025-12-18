# Summarize a data frame as binned interpolated percentiles

**\[superseded\]**

This function was superseded and EPI insteads uses
[`averaged_quantile()`](https://economic.github.io/epidatatools/reference/averaged_quantile.md)
to interpolate percentiles.

## Usage

``` r
binipolate(data, x, probs = 0.5, bin_size, .by = NULL, w = NULL)
```

## Arguments

- data:

  data frame

- x:

  column to compute

- probs:

  numeric vector of percentiles with values `[0,1]`

- bin_size:

  size of binning

- .by:

  optional, a tidy-selection of columns for single-operation grouping

- w:

  numeric vector of weights the same length as x giving the weights to
  use for elements of x

## Value

a tibble or data frame

## Examples

``` r
binipolate(mtcars, mpg, bin_size = 0.25)
#> # A tibble: 1 × 2
#>   probs value
#>   <dbl> <dbl>
#> 1   0.5  19.1
binipolate(mtcars, mpg, probs = c(0.25, 0.5, 0.75), bin_size = 0.25)
#> # A tibble: 3 × 2
#>   probs value
#>   <dbl> <dbl>
#> 1  0.25  15.4
#> 2  0.5   19.1
#> 3  0.75  22.2
binipolate(mtcars, mpg, probs = c(0.25, 0.5, 0.75), bin_size = 0.25, .by = cyl, w = wt)
#> # A tibble: 9 × 3
#>     cyl probs value
#>   <dbl> <dbl> <dbl>
#> 1     4  0.25  21.9
#> 2     4  0.5   23.9
#> 3     4  0.75  28.3
#> 4     6  0.25  18.0
#> 5     6  0.5   19.5
#> 6     6  0.75  20.6
#> 7     8  0.25  13.0
#> 8     8  0.5   15.2
#> 9     8  0.75  16.1
```
