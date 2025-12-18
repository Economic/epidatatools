# Join data frames and create a merge indicator

Join data frames and create a merge indicator

## Usage

``` r
merge_status(x, y, ...)

# S3 method for class 'data.frame'
merge_status(x, y, ...)
```

## Arguments

- x, y:

  data frames

- ...:

  passed to dplyr::full_join()

## Value

a merged data frame from full_join with an extra column `_merge`

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
merge_status(band_members, band_instruments, by = "name")
#> # A tibble: 4 × 4
#>   name  band    plays  `_merge`  
#>   <chr> <chr>   <chr>  <chr>     
#> 1 Mick  Stones  NA     left_only 
#> 2 John  Beatles guitar both      
#> 3 Paul  Beatles bass   both      
#> 4 Keith NA      guitar right_only
```
