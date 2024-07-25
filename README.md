
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epidatatools <a href="https://economic.github.io/epidatatools/"><img src="man/figures/logo.png" align="right" height="139" /></a>

epidatatools contains functions we find useful at
[EPI](https://epi.org/) that don’t have exact analogues elsewhere in the
R package ecosystem.

## One and two-way cross-tabulations

``` r
library(epidatatools)

crosstab(mtcars, cyl)
#> # A tibble: 3 × 4
#>     cyl     n percent cumul_percent
#>   <dbl> <int>   <dbl>         <dbl>
#> 1     4    11   0.344         0.344
#> 2     6     7   0.219         0.562
#> 3     8    14   0.438         1
```

``` r

crosstab(mtcars, cyl, gear)
#> # A tibble: 3 × 4
#>     cyl   `3`   `4`   `5`
#>   <dbl> <int> <int> <int>
#> 1     4     1     8     2
#> 2     6     2     4     1
#> 3     8    12     0     2
```

``` r

# weighted counts expressed as column percentages
crosstab(mtcars, cyl, gear, w = mpg, percent = "column")
#> # A tibble: 3 × 4
#>     cyl    `3`   `4`   `5`
#>   <dbl>  <dbl> <dbl> <dbl>
#> 1     4 0.0890 0.732 0.528
#> 2     6 0.163  0.268 0.184
#> 3     8 0.748  0     0.288
```

## Join data frames and add a merge status indicator

``` r
library(dplyr)

merge_status(band_members, band_instruments, by = "name")
#> # A tibble: 4 × 4
#>   name  band    plays  `_merge`  
#>   <chr> <chr>   <chr>  <chr>     
#> 1 Mick  Stones  <NA>   left_only 
#> 2 John  Beatles guitar both      
#> 3 Paul  Beatles bass   both      
#> 4 Keith <NA>    guitar right_only
```

## Summarize across several distinct groups

``` r
summarize_groups(mtcars, cyl|gear|carb, p50 = median(mpg), avg = mean(hp))
#> # A tibble: 12 × 4
#>    group_name group_value   p50   avg
#>    <chr>            <dbl> <dbl> <dbl>
#>  1 cyl                  4  26    82.6
#>  2 cyl                  6  19.7 122. 
#>  3 cyl                  8  15.2 209. 
#>  4 gear                 3  15.5 176. 
#>  5 gear                 4  22.8  89.5
#>  6 gear                 5  19.7 196. 
#>  7 carb                 1  22.8  86  
#>  8 carb                 2  22.1 117. 
#>  9 carb                 3  16.4 180  
#> 10 carb                 4  15.2 187  
#> 11 carb                 6  19.7 175  
#> 12 carb                 8  15   335
```

## Installation

Install the latest version from R-Universe:

``` r
install.packages("epidatatools", repos = c("https://economic.r-universe.dev", "https://cloud.r-project.org"))
```
