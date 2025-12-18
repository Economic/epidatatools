# Find BLS Series by Search String

Searches the [BLS Data Finder](https://data.bls.gov/dataQuery/search)
for series matching a search string.

Searches the [BLS Data Finder](https://data.bls.gov/dataQuery/search)
for series matching a search string.

## Usage

``` r
find_bls(
  search_string,
  max_results = 20,
  metadata = FALSE,
  survey = NULL,
  seasonality = NULL
)

find_bls(
  search_string,
  max_results = 20,
  metadata = FALSE,
  survey = NULL,
  seasonality = NULL
)
```

## Arguments

- search_string:

  Character string to search for in series titles

- max_results:

  Maximum number of results to return (default: 20)

- metadata:

  Logical flag to retrieve catalog metadata (default: FALSE). When
  FALSE, only series_id and series_title are returned for faster
  performance.

- survey:

  Character string specifying a BLS survey code to restrict the search
  (default: NULL). When NULL, searches across all surveys. See [BLS Data
  Finder](https://data.bls.gov/dataQuery/find?removeAll=1) for available
  survey codes (e.g., "cw" for CPI Urban Wage Earners, "ln" for Labor
  Force Statistics, "ce" for Current Employment Statistics).

- seasonality:

  Character string to filter by seasonal adjustment (default: NULL).
  When NULL, no filter is applied. Use "SA" to restrict to seasonally
  adjusted series, or "NSA" to restrict to not seasonally adjusted
  series.

## Value

A tibble with columns series_id, series_title, and optionally metadata
(a list column containing catalog metadata as one-row tibbles when
metadata = TRUE)

A tibble with columns series_id, series_title, and optionally metadata
(a list column containing catalog metadata as one-row tibbles when
metadata = TRUE)

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("BLS_API_KEY"))
find_bls("unemployment rate")

find_bls("wage", survey = "cw")
}
if (FALSE) {
find_bls("unemployment rate")

find_bls("wage", survey = "cw")
}
```
