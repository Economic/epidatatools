# Retrieve data from the FRED API

This function is simply a wrapper around
[`fredr`](https://sboysel.github.io/fredr/) and requires you to have an
[FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html).

## Usage

``` r
get_fred(
  series,
  start = NULL,
  end = NULL,
  metadata = FALSE,
  fred_api_key = Sys.getenv("FRED_API_KEY")
)
```

## Arguments

- series:

  FRED series code

- start:

  Start year or date (numeric year or Date object)

- end:

  End year or date (numeric year or Date object)

- metadata:

  Flag for additional metadata

- fred_api_key:

  FRED API key (defaults to FRED_API_KEY environment variable)

## Value

A tibble

## Examples

``` r
get_fred("UNRATE")
#> # A tibble: 935 × 7
#>    series_id series_title      date_frequency date        year month value
#>    <chr>     <chr>             <chr>          <date>     <dbl> <dbl> <dbl>
#>  1 UNRATE    Unemployment Rate month          1948-01-01  1948     1   3.4
#>  2 UNRATE    Unemployment Rate month          1948-02-01  1948     2   3.8
#>  3 UNRATE    Unemployment Rate month          1948-03-01  1948     3   4  
#>  4 UNRATE    Unemployment Rate month          1948-04-01  1948     4   3.9
#>  5 UNRATE    Unemployment Rate month          1948-05-01  1948     5   3.5
#>  6 UNRATE    Unemployment Rate month          1948-06-01  1948     6   3.6
#>  7 UNRATE    Unemployment Rate month          1948-07-01  1948     7   3.6
#>  8 UNRATE    Unemployment Rate month          1948-08-01  1948     8   3.9
#>  9 UNRATE    Unemployment Rate month          1948-09-01  1948     9   3.8
#> 10 UNRATE    Unemployment Rate month          1948-10-01  1948    10   3.7
#> # ℹ 925 more rows

series = c(
  gdp = "GDP",
  urate = "UNRATE"
)
get_fred(series, start = as.Date("2024-07-01"), end = 2024)
#> # A tibble: 16 × 9
#>    name  series_id series_title    date_frequency date        year quarter month
#>    <chr> <chr>     <chr>           <chr>          <date>     <dbl>   <int> <dbl>
#>  1 gdp   GDP       Gross Domestic… quarter        2024-07-01  2024       3     7
#>  2 gdp   GDP       Gross Domestic… quarter        2024-10-01  2024       4    10
#>  3 urate UNRATE    Unemployment R… month          2024-07-01  2024       3     7
#>  4 urate UNRATE    Unemployment R… month          2024-08-01  2024       3     8
#>  5 urate UNRATE    Unemployment R… month          2024-09-01  2024       3     9
#>  6 urate UNRATE    Unemployment R… month          2024-10-01  2024       4    10
#>  7 urate UNRATE    Unemployment R… month          2024-11-01  2024       4    11
#>  8 urate UNRATE    Unemployment R… month          2024-12-01  2024       4    12
#>  9 gdp   GDP       Gross Domestic… quarter        2024-07-01  2024       3     7
#> 10 gdp   GDP       Gross Domestic… quarter        2024-10-01  2024       4    10
#> 11 urate UNRATE    Unemployment R… month          2024-07-01  2024       3     7
#> 12 urate UNRATE    Unemployment R… month          2024-08-01  2024       3     8
#> 13 urate UNRATE    Unemployment R… month          2024-09-01  2024       3     9
#> 14 urate UNRATE    Unemployment R… month          2024-10-01  2024       4    10
#> 15 urate UNRATE    Unemployment R… month          2024-11-01  2024       4    11
#> 16 urate UNRATE    Unemployment R… month          2024-12-01  2024       4    12
#> # ℹ 1 more variable: value <dbl>

complete_results = get_fred(series, start = 2020, end = 2025, metadata = TRUE)
complete_results
#> # A tibble: 2 × 4
#>   name  series_id metadata          data             
#>   <chr> <chr>     <list>            <list>           
#> 1 gdp   GDP       <tibble [1 × 15]> <tibble [22 × 2]>
#> 2 urate UNRATE    <tibble [1 × 15]> <tibble [71 × 2]>

complete_results |>
  tidyr::unnest(metadata)
#> # A tibble: 2 × 18
#>   name  series_id id     realtime_start realtime_end title     observation_start
#>   <chr> <chr>     <chr>  <chr>          <chr>        <chr>     <chr>            
#> 1 gdp   GDP       GDP    2025-09-29     2025-09-29   Gross Do… 1947-01-01       
#> 2 urate UNRATE    UNRATE 2025-12-16     2025-12-16   Unemploy… 1948-01-01       
#> # ℹ 11 more variables: observation_end <chr>, frequency <chr>,
#> #   frequency_short <chr>, units <chr>, units_short <chr>,
#> #   seasonal_adjustment <chr>, seasonal_adjustment_short <chr>,
#> #   last_updated <chr>, popularity <int>, notes <chr>, data <list>
```
