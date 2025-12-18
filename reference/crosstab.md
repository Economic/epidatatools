# Cross-tabulate one or two variables

Cross-tabulate one or two variables

## Usage

``` r
crosstab(data, ..., w = NULL, percent = NULL)
```

## Arguments

- data:

  a data frame

- ...:

  one or two variables, for a one- or two-way cross-tabulation

- w:

  weight

- percent:

  for a two-way cross-tabulation, replace counts with row or column
  percentages

  - NULL, the default, shows counts rather than percentages

  - "row" will replace counts with row percentages, summing to 100%
    across columns within each row

  - "column" will replace counts with column percentages, adding to 100%
    across rows within each column

## Value

a tibble

## Examples

``` r
crosstab(mtcars, cyl)
#> # A tibble: 3 × 4
#>     cyl     n percent cumul_percent
#>   <dbl> <int>   <dbl>         <dbl>
#> 1     4    11   0.344         0.344
#> 2     6     7   0.219         0.562
#> 3     8    14   0.438         1    
crosstab(mtcars, cyl, gear)
#> # A tibble: 3 × 4
#>     cyl   `3`   `4`   `5`
#>   <dbl> <int> <int> <int>
#> 1     4     1     8     2
#> 2     6     2     4     1
#> 3     8    12     0     2
crosstab(mtcars, cyl, gear, w = mpg, percent = "column")
#> # A tibble: 3 × 4
#>     cyl    `3`   `4`   `5`
#>   <dbl>  <dbl> <dbl> <dbl>
#> 1     4 0.0890 0.732 0.528
#> 2     6 0.163  0.268 0.184
#> 3     8 0.748  0     0.288
```
