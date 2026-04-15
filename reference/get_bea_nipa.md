# Retrieve NIPA data from the BEA API

Retrieves National Income and Product Accounts data from the [Bureau of
Economic Analysis API](https://apps.bea.gov/API/signup/). Requires a BEA
API key saved in the `BEA_API_KEY` environment variable.

## Usage

``` r
get_bea_nipa(
  tables,
  years,
  frequency = c("year", "quarter", "month"),
  underlying = FALSE,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
)
```

## Arguments

- tables:

  Character vector of NIPA table names (e.g., "T10101", "T20305"). Can
  be a named vector to add custom names.

- years:

  Numeric vector of years or "ALL" for all available years

- frequency:

  Character string: "year" (annual), "quarter" (quarterly), or "month"
  (monthly). Can specify multiple as comma-separated string.

- underlying:

  Logical flag to use NIUnderlyingDetail dataset instead of NIPA
  (default: FALSE)

- metadata:

  Logical flag to return additional metadata columns (default: FALSE)

- bea_api_key:

  BEA API key (defaults to BEA_API_KEY environment variable)

## Value

A tibble with columns: table_name, table_description, line_number,
line_description, date_frequency, date, value, and date variables (year,
quarter, month) appropriate to the frequency of data returned. Annual
data includes only year; quarterly data includes year and quarter;
monthly data includes year, quarter, and month. If metadata = TRUE, also
includes unit_mult, metric_name, cl_unit, series_code, and note_text
(list column). If tables is a named vector, includes a "name" column as
the first column.

## Examples

``` r
get_bea_nipa("T10101", years = 2020:2024, frequency = "quarter")
#> # A tibble: 500 × 9
#>    table_name table_description      line_number line_description date_frequency
#>    <chr>      <chr>                        <int> <chr>            <chr>         
#>  1 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  2 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  3 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  4 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  5 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  6 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  7 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  8 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#>  9 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#> 10 T10101     Table 1.1.1. Percent …           1 Gross domestic … quarter       
#> # ℹ 490 more rows
#> # ℹ 4 more variables: date <date>, year <int>, quarter <int>, value <dbl>

get_bea_nipa(
  c("gdp" = "T10101", "personal_income" = "T20305"),
  years = 2023:2024,
  frequency = "year"
)
#> # A tibble: 112 × 9
#>    name  table_name table_description               line_number line_description
#>    <chr> <chr>      <chr>                                 <int> <chr>           
#>  1 gdp   T10101     Table 1.1.1. Percent Change Fr…           1 Gross domestic …
#>  2 gdp   T10101     Table 1.1.1. Percent Change Fr…           1 Gross domestic …
#>  3 gdp   T10101     Table 1.1.1. Percent Change Fr…           2 Personal consum…
#>  4 gdp   T10101     Table 1.1.1. Percent Change Fr…           2 Personal consum…
#>  5 gdp   T10101     Table 1.1.1. Percent Change Fr…           3 Goods           
#>  6 gdp   T10101     Table 1.1.1. Percent Change Fr…           3 Goods           
#>  7 gdp   T10101     Table 1.1.1. Percent Change Fr…           4 Durable goods   
#>  8 gdp   T10101     Table 1.1.1. Percent Change Fr…           4 Durable goods   
#>  9 gdp   T10101     Table 1.1.1. Percent Change Fr…           5 Nondurable goods
#> 10 gdp   T10101     Table 1.1.1. Percent Change Fr…           5 Nondurable goods
#> # ℹ 102 more rows
#> # ℹ 4 more variables: date_frequency <chr>, date <date>, year <int>,
#> #   value <dbl>

get_bea_nipa("T10101", years = 2023, frequency = "year", metadata = TRUE)
#> # A tibble: 25 × 15
#>    table_name table_description      line_number line_description date_frequency
#>    <chr>      <chr>                        <int> <chr>            <chr>         
#>  1 T10101     Table 1.1.1. Percent …           1 Gross domestic … year          
#>  2 T10101     Table 1.1.1. Percent …           2 Personal consum… year          
#>  3 T10101     Table 1.1.1. Percent …           3 Goods            year          
#>  4 T10101     Table 1.1.1. Percent …           4 Durable goods    year          
#>  5 T10101     Table 1.1.1. Percent …           5 Nondurable goods year          
#>  6 T10101     Table 1.1.1. Percent …           6 Services         year          
#>  7 T10101     Table 1.1.1. Percent …           7 Gross private d… year          
#>  8 T10101     Table 1.1.1. Percent …           8 Fixed investment year          
#>  9 T10101     Table 1.1.1. Percent …           9 Nonresidential   year          
#> 10 T10101     Table 1.1.1. Percent …          10 Structures       year          
#> # ℹ 15 more rows
#> # ℹ 10 more variables: date <date>, year <int>, quarter <int>, month <int>,
#> #   value <dbl>, unit_mult <int>, metric_name <chr>, cl_unit <chr>,
#> #   series_code <chr>, note_text <list>
```
