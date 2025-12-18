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
if (FALSE) { # nzchar(Sys.getenv("BEA_API_KEY"))
get_bea_regional(geo_fips = "STATE", table_name = "SAINC1", line_code = 1)

get_bea_regional(geo_fips = c("36000", "06000"), table_name = "SAINC1", line_code = 1)
}
```
