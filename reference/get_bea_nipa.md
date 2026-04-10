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
#> Error in purrr::map(seq_along(tables), function(idx) {    result = fetch_bea_nipa_complete(idx, tables, years_param,         freq_param, dataset_name, bea_api_key)    if (!is.null(table_names)) {        nm = table_names[idx]        result = dplyr::mutate(result, name = dplyr::if_else(nm ==             "", NA_character_, nm), .before = 1)    }    result}): ℹ In index: 1.
#> Caused by error in `get_bea_api()`:
#> ! Forbidden (HTTP 403).

get_bea_nipa(
  c("gdp" = "T10101", "personal_income" = "T20305"),
  years = 2023:2024,
  frequency = "year"
)
#> Error in purrr::map(seq_along(tables), function(idx) {    result = fetch_bea_nipa_complete(idx, tables, years_param,         freq_param, dataset_name, bea_api_key)    if (!is.null(table_names)) {        nm = table_names[idx]        result = dplyr::mutate(result, name = dplyr::if_else(nm ==             "", NA_character_, nm), .before = 1)    }    result}): ℹ In index: 1.
#> Caused by error in `get_bea_api()`:
#> ! Forbidden (HTTP 403).

get_bea_nipa("T10101", years = 2023, frequency = "year", metadata = TRUE)
#> Error in purrr::map(seq_along(tables), function(idx) {    result = fetch_bea_nipa_complete(idx, tables, years_param,         freq_param, dataset_name, bea_api_key)    if (!is.null(table_names)) {        nm = table_names[idx]        result = dplyr::mutate(result, name = dplyr::if_else(nm ==             "", NA_character_, nm), .before = 1)    }    result}): ℹ In index: 1.
#> Caused by error in `get_bea_api()`:
#> ! Forbidden (HTTP 403).
```
