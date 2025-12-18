# Calculate the averaged (smoothed) median

Calculate the averaged (smoothed) median

## Usage

``` r
averaged_median(
  x,
  w = NULL,
  na.rm = TRUE,
  quantiles_n = 9L,
  quantiles_w = c(1:4, 5, 4:1)
)
```

## Arguments

- x:

  numeric vector or an R object

- w:

  numeric vector of sample weights the same length as x giving the
  weights to use for elements of x

- na.rm:

  logical; if true, any NA or NaN's are removed from x before
  computation

- quantiles_n:

  integer number of quantiles used for averaging; must be odd

- quantiles_w:

  weights used for average quantiles; length must equal quantiles_n

## Value

a scalar

## Examples

``` r
averaged_median(x = mtcars$mpg)
#> [1] 19.2
```
