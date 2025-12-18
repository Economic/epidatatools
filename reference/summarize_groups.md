# Summarize distinct groups

Summarize distinct groups

## Usage

``` r
summarize_groups(.data, .groups, ...)
```

## Arguments

- .data:

  a data frame

- .groups:

  grouping variables as a tidy selection specification of columns, as
  used in
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

- ...:

  name-value pairs passed to dplyr::summarize()

## Value

a tibble

## Examples

``` r
summarize_groups(mtcars, cyl|gear|carb, median(mpg), mean(hp))
#> # A tibble: 12 × 4
#>    group_name group_value `median(mpg)` `mean(hp)`
#>    <chr>            <dbl>         <dbl>      <dbl>
#>  1 cyl                  4          26         82.6
#>  2 cyl                  6          19.7      122. 
#>  3 cyl                  8          15.2      209. 
#>  4 gear                 3          15.5      176. 
#>  5 gear                 4          22.8       89.5
#>  6 gear                 5          19.7      196. 
#>  7 carb                 1          22.8       86  
#>  8 carb                 2          22.1      117. 
#>  9 carb                 3          16.4      180  
#> 10 carb                 4          15.2      187  
#> 11 carb                 6          19.7      175  
#> 12 carb                 8          15        335  
```
