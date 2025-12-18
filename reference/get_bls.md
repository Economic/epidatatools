# Retrieve data from the BLS API

This function is simply a wrapper around
[`blsR`](https://github.com/groditi/blsR) and requires you to have an
[BLS API key](https://www.bls.gov/developers/) saved in the
`BLS_API_KEY` environment variable.

## Usage

``` r
get_bls(
  series,
  start,
  end,
  metadata = FALSE,
  bls_api_key = Sys.getenv("BLS_API_KEY")
)
```

## Arguments

- series:

  BLS series code

- start:

  Start year (numeric)

- end:

  End year (numeric)

- metadata:

  Flag for additional metadata

- bls_api_key:

  BLS API key (defaults to BLS_API_KEY environment variable)

## Value

a tibble

## Examples

``` r
get_bls("LNU02300060", start = 2020, end = 2024)
#> # A tibble: 60 × 7
#>    series_id   series_title          date_frequency date        year month value
#>    <chr>       <chr>                 <chr>          <date>     <dbl> <dbl> <dbl>
#>  1 LNU02300060 (Unadj) Employment-P… month          2020-01-01  2020     1  80.2
#>  2 LNU02300060 (Unadj) Employment-P… month          2020-02-01  2020     2  80.3
#>  3 LNU02300060 (Unadj) Employment-P… month          2020-03-01  2020     3  79.5
#>  4 LNU02300060 (Unadj) Employment-P… month          2020-04-01  2020     4  69.8
#>  5 LNU02300060 (Unadj) Employment-P… month          2020-05-01  2020     5  71.5
#>  6 LNU02300060 (Unadj) Employment-P… month          2020-06-01  2020     6  73.3
#>  7 LNU02300060 (Unadj) Employment-P… month          2020-07-01  2020     7  73.4
#>  8 LNU02300060 (Unadj) Employment-P… month          2020-08-01  2020     8  75  
#>  9 LNU02300060 (Unadj) Employment-P… month          2020-09-01  2020     9  75.4
#> 10 LNU02300060 (Unadj) Employment-P… month          2020-10-01  2020    10  76.4
#> # ℹ 50 more rows

bls_series_ids = c(
  emp_fb_2534 = "LNU02073399",
  epop_asianmen_2554 = "LNU02332330Q",
  cpi_semi = "CUUS0000SA0",
  "LNU02300060"
)
get_bls(bls_series_ids, start = 2024, end = 2024)
#> # A tibble: 120 × 10
#>    name  series_id series_title date_frequency date        year semiyear quarter
#>    <chr> <chr>     <chr>        <chr>          <date>     <dbl>    <dbl>   <int>
#>  1 emp_… LNU02073… (Unadj) Emp… month          2024-01-01  2024        1       1
#>  2 emp_… LNU02073… (Unadj) Emp… month          2024-02-01  2024        1       1
#>  3 emp_… LNU02073… (Unadj) Emp… month          2024-03-01  2024        1       1
#>  4 emp_… LNU02073… (Unadj) Emp… month          2024-04-01  2024        1       2
#>  5 emp_… LNU02073… (Unadj) Emp… month          2024-05-01  2024        1       2
#>  6 emp_… LNU02073… (Unadj) Emp… month          2024-06-01  2024        1       2
#>  7 emp_… LNU02073… (Unadj) Emp… month          2024-07-01  2024        2       3
#>  8 emp_… LNU02073… (Unadj) Emp… month          2024-08-01  2024        2       3
#>  9 emp_… LNU02073… (Unadj) Emp… month          2024-09-01  2024        2       3
#> 10 emp_… LNU02073… (Unadj) Emp… month          2024-10-01  2024        2       4
#> # ℹ 110 more rows
#> # ℹ 2 more variables: month <dbl>, value <dbl>

complete_results = get_bls(bls_series_ids, start = 2024, end = 2024, metadata = TRUE)
complete_results
#> # A tibble: 4 × 4
#>   name               series_id    metadata          data             
#>   <chr>              <chr>        <list>            <list>           
#> 1 emp_fb_2534        LNU02073399  <tibble [1 × 13]> <tibble [12 × 7]>
#> 2 epop_asianmen_2554 LNU02332330Q <tibble [1 × 13]> <tibble [4 × 7]> 
#> 3 cpi_semi           CUUS0000SA0  <tibble [1 × 7]>  <tibble [2 × 7]> 
#> 4 NA                 LNU02300060  <tibble [1 × 13]> <tibble [12 × 7]>

complete_results |>
  tidyr::unnest(metadata)
#> # A tibble: 4 × 18
#>   name        series_id series_title seasonality survey_name survey_abbreviation
#>   <chr>       <chr>     <chr>        <chr>       <chr>       <chr>              
#> 1 emp_fb_2534 LNU02073… (Unadj) Emp… Not Season… Labor Forc… LN                 
#> 2 epop_asian… LNU02332… (Unadj) Emp… Not Season… Labor Forc… LN                 
#> 3 cpi_semi    CUUS0000… All items i… Not Season… Consumer P… CU                 
#> 4 NA          LNU02300… (Unadj) Emp… Not Season… Labor Forc… LN                 
#> # ℹ 12 more variables: measure_data_type <chr>, commerce_industry <chr>,
#> #   occupation <chr>, cps_labor_force_status <chr>, demographic_age <chr>,
#> #   demographic_ethnic_origin <chr>, demographic_race <chr>,
#> #   demographic_gender <chr>, demographic_education <chr>, area <chr>,
#> #   item <chr>, data <list>
```
