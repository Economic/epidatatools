# Calculate the averaged (smoothed) quantile

Calculate the averaged (smoothed) quantile

## Usage

``` r
averaged_quantile(
  x,
  w = NULL,
  probs = 0.5,
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

- probs:

  numeric; percentile with value `[0,1]`

- na.rm:

  logical; if true, any NA or NaN's are removed from x before
  computation

- quantiles_n:

  integer number of quantiles used for averaging; must be odd

- quantiles_w:

  weights used for average quantiles; length must equal quantiles_n

## Value

a numeric vector with length probs

## Examples

``` r
averaged_quantile(x = mtcars$mpg, probs = c(0.25, 0.5, 0.75))
#> [1] 15.392 19.200 22.812
```
