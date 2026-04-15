# Retrieve Regional data from the BEA API

Retrieves regional economic data from the [Bureau of Economic Analysis
API](https://apps.bea.gov/API/signup/). Requires a BEA API key saved in
the `BEA_API_KEY` environment variable.

## Usage

``` r
get_bea_regional(
  geo_fips,
  table_name,
  line_code,
  year = NULL,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
)
```

## Arguments

- geo_fips:

  Character vector of geographic FIPS codes, or special values: "COUNTY"
  for all counties, "STATE" for all states, "MSA" for all MSAs. A single
  value can also be a state abbreviation (e.g., "NY", "CA").

- table_name:

  Single table name (e.g., "CAINC1" for personal income)

- line_code:

  Single line code specifying the statistic to retrieve, or "ALL" for
  all line codes

- year:

  Optional numeric vector of years or "ALL" for all available years.
  Defaults to "LAST5".

- metadata:

  Logical flag to return additional metadata columns (default: FALSE).
  When TRUE, also includes cl_unit, mult_unit, and note_text (list
  column).

- bea_api_key:

  BEA API key (defaults to BEA_API_KEY environment variable)

## Value

A tibble with columns: geo_fips, geo_name, table_name,
table_description, line_number, line_description, date_frequency, date,
year, value. If metadata = TRUE, also includes cl_unit, mult_unit, and
note_text (list column containing all notes from the API response).

## Examples

``` r
get_bea_regional(geo_fips = "STATE", table_name = "SAINC1", line_code = 1)
#> # A tibble: 300 × 10
#>    geo_fips geo_name   table_name table_description line_number line_description
#>    <chr>    <chr>      <chr>      <chr>                   <int> <chr>           
#>  1 00000    United St… SAINC1     SAINC1 State ann…           1 Personal income 
#>  2 00000    United St… SAINC1     SAINC1 State ann…           1 Personal income 
#>  3 00000    United St… SAINC1     SAINC1 State ann…           1 Personal income 
#>  4 00000    United St… SAINC1     SAINC1 State ann…           1 Personal income 
#>  5 00000    United St… SAINC1     SAINC1 State ann…           1 Personal income 
#>  6 01000    Alabama    SAINC1     SAINC1 State ann…           1 Personal income 
#>  7 01000    Alabama    SAINC1     SAINC1 State ann…           1 Personal income 
#>  8 01000    Alabama    SAINC1     SAINC1 State ann…           1 Personal income 
#>  9 01000    Alabama    SAINC1     SAINC1 State ann…           1 Personal income 
#> 10 01000    Alabama    SAINC1     SAINC1 State ann…           1 Personal income 
#> # ℹ 290 more rows
#> # ℹ 4 more variables: date_frequency <chr>, date <date>, year <int>,
#> #   value <dbl>

get_bea_regional(geo_fips = c("36000", "06000"), table_name = "SAINC1", line_code = 1)
#> # A tibble: 10 × 10
#>    geo_fips geo_name   table_name table_description line_number line_description
#>    <chr>    <chr>      <chr>      <chr>                   <int> <chr>           
#>  1 06000    California SAINC1     SAINC1 State ann…           1 Personal income 
#>  2 06000    California SAINC1     SAINC1 State ann…           1 Personal income 
#>  3 06000    California SAINC1     SAINC1 State ann…           1 Personal income 
#>  4 06000    California SAINC1     SAINC1 State ann…           1 Personal income 
#>  5 06000    California SAINC1     SAINC1 State ann…           1 Personal income 
#>  6 36000    New York   SAINC1     SAINC1 State ann…           1 Personal income 
#>  7 36000    New York   SAINC1     SAINC1 State ann…           1 Personal income 
#>  8 36000    New York   SAINC1     SAINC1 State ann…           1 Personal income 
#>  9 36000    New York   SAINC1     SAINC1 State ann…           1 Personal income 
#> 10 36000    New York   SAINC1     SAINC1 State ann…           1 Personal income 
#> # ℹ 4 more variables: date_frequency <chr>, date <date>, year <int>,
#> #   value <dbl>
```
