
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epidatatools <a href="https://economic.github.io/epidatatools/"><img src="man/figures/logo.png" align="right" height="139"/></a>

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

crosstab(mtcars, cyl, gear)
#> # A tibble: 3 × 4
#>     cyl   `3`   `4`   `5`
#>   <dbl> <int> <int> <int>
#> 1     4     1     8     2
#> 2     6     2     4     1
#> 3     8    12     0     2

# weighted counts expressed as column percentages
crosstab(mtcars, cyl, gear, w = mpg, percent = "column")
#> # A tibble: 3 × 4
#>     cyl    `3`   `4`   `5`
#>   <dbl>  <dbl> <dbl> <dbl>
#> 1     4 0.0890 0.732 0.528
#> 2     6 0.163  0.268 0.184
#> 3     8 0.748  0     0.288
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

## Calculate averaged (smoothed) quantiles

``` r
averaged_median(mtcars$mpg)
#> [1] 19.2

p = c(10, 50, 90)

mtcars |>
  dplyr::reframe(
    p,
    value = averaged_quantile(mpg, probs = p / 100)
  )
#>    p  value
#> 1 10 13.832
#> 2 50 19.200
#> 3 90 30.108
```

## Download IPUMS microdata

Requires valid IPUMS API key stored in the .Renviron as `IPUMS_API_KEY`.

``` r
# First make sure you have a valid IPUMS API key stored in your .Renviron file as `IPUMS_API_KEY`.

# CPS ASEC microdata
dl_ipums_asec(2021:2022, c("YEAR", "OFFPOV", "ASECWT"))

#> DDI codebook file saved to /tmp/RtmpGaUUhf/cps_00161.xml
#> Data file saved to /tmp/RtmpGaUUhf/cps_00161.dat.gz
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> # A tibble: 316,275 × 11
#>     YEAR SERIAL MONTH       CPSID ASECFLAG ASECWTH PERNUM  CPSIDP  CPSIDV ASECWT
#>    <dbl>  <dbl> <int+lbl>   <dbl> <int+lb>   <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
#>  1  2021      1 3 [March] 2.02e13 1 [ASEC]    706.      1 2.02e13 2.02e14   706.
#>  2  2021      1 3 [March] 2.02e13 1 [ASEC]    706.      2 2.02e13 2.02e14   706.
#>  3  2021      1 3 [March] 2.02e13 1 [ASEC]    706.      3 2.02e13 2.02e14   608.
#>  4  2021      2 3 [March] 2.02e13 1 [ASEC]   1537.      1 2.02e13 2.02e14  1537.
#>  5  2021      2 3 [March] 2.02e13 1 [ASEC]   1537.      2 2.02e13 2.02e14  1537.
#>  6  2021      2 3 [March] 2.02e13 1 [ASEC]   1537.      3 2.02e13 2.02e14  2119.
#>  7  2021      3 3 [March] 2.02e13 1 [ASEC]   1499.      1 2.02e13 2.02e14  1499.
#>  8  2021      8 3 [March] 2.02e13 1 [ASEC]    948.      1 2.02e13 2.02e14   948.
#>  9  2021      8 3 [March] 2.02e13 1 [ASEC]    948.      2 2.02e13 2.02e14  1225.
#> 10  2021      8 3 [March] 2.02e13 1 [ASEC]    948.      3 2.02e13 2.02e14   948.
#> # ℹ 316,265 more rows
#> # ℹ 1 more variable: OFFPOV <int+lbl>
```

## Get data from the BLS API

Requires valid BLS or FRED API keys stored in the .Renviron as
`IPUMS_API_KEY`.

``` r
# To run this example you will need a BLS API key 
# in your .Renviron file named BLS_API_KEY

bls_series_ids = c(
  prime_age_epop = "LNS12300060",
  injuries_annual = "ISU00000000031004"
)

get_bls(bls_series_ids, start = 2023, end = 2023)
#> # A tibble: 13 × 8
#>    name       series_id series_title date_frequency date        year month value
#>    <chr>      <chr>     <chr>        <chr>          <date>     <dbl> <dbl> <dbl>
#>  1 prime_age… LNS12300… (Seas) Empl… month          2023-01-01  2023     1  80.3
#>  2 prime_age… LNS12300… (Seas) Empl… month          2023-02-01  2023     2  80.5
#>  3 prime_age… LNS12300… (Seas) Empl… month          2023-03-01  2023     3  80.7
#>  4 prime_age… LNS12300… (Seas) Empl… month          2023-04-01  2023     4  80.7
#>  5 prime_age… LNS12300… (Seas) Empl… month          2023-05-01  2023     5  80.7
#>  6 prime_age… LNS12300… (Seas) Empl… month          2023-06-01  2023     6  80.9
#>  7 prime_age… LNS12300… (Seas) Empl… month          2023-07-01  2023     7  80.9
#>  8 prime_age… LNS12300… (Seas) Empl… month          2023-08-01  2023     8  80.8
#>  9 prime_age… LNS12300… (Seas) Empl… month          2023-09-01  2023     9  80.8
#> 10 prime_age… LNS12300… (Seas) Empl… month          2023-10-01  2023    10  80.6
#> 11 prime_age… LNS12300… (Seas) Empl… month          2023-11-01  2023    11  80.7
#> 12 prime_age… LNS12300… (Seas) Empl… month          2023-12-01  2023    12  80.5
#> 13 injuries_… ISU00000… Industry-le… year           2023-01-01  2023     1   2.6
```

## Get data from the FRED API

``` r
# To run this example you will need a FRED API key 
# in your .Renviron named FRED_API_KEY

fred_series_ids = c(
  gdp = "GDP",
  unemployment_rate = "UNRATE"
)

get_fred(fred_series_ids, start = as.Date("2024-06-01"), end = 2024)
#> # A tibble: 10 × 9
#>    name     series_id series_title date_frequency date        year quarter month
#>    <chr>    <chr>     <chr>        <chr>          <date>     <dbl>   <int> <dbl>
#>  1 gdp      GDP       Gross Domes… quarter        2024-04-01  2024       2     4
#>  2 gdp      GDP       Gross Domes… quarter        2024-07-01  2024       3     7
#>  3 gdp      GDP       Gross Domes… quarter        2024-10-01  2024       4    10
#>  4 unemplo… UNRATE    Unemploymen… month          2024-06-01  2024       2     6
#>  5 unemplo… UNRATE    Unemploymen… month          2024-07-01  2024       3     7
#>  6 unemplo… UNRATE    Unemploymen… month          2024-08-01  2024       3     8
#>  7 unemplo… UNRATE    Unemploymen… month          2024-09-01  2024       3     9
#>  8 unemplo… UNRATE    Unemploymen… month          2024-10-01  2024       4    10
#>  9 unemplo… UNRATE    Unemploymen… month          2024-11-01  2024       4    11
#> 10 unemplo… UNRATE    Unemploymen… month          2024-12-01  2024       4    12
#> # ℹ 1 more variable: value <dbl>
```

## Installation

Install the latest version from R-Universe:

``` r
install.packages("epidatatools", repos = c("https://economic.r-universe.dev", "https://cloud.r-project.org"))
```
