# Find FRED Series by Search String

Searches the [FRED database](https://fred.stlouisfed.org/) for series
matching a search string.

## Usage

``` r
find_fred(
  search_string,
  max_results = 20,
  metadata = FALSE,
  seasonality = NULL,
  fred_api_key = Sys.getenv("FRED_API_KEY")
)
```

## Arguments

- search_string:

  Character string to search for in series titles

- max_results:

  Maximum number of results to return (default: 20)

- metadata:

  Logical flag to retrieve additional metadata (default: FALSE). When
  FALSE, only series_id and series_title are returned.

- seasonality:

  Optional filter for seasonal adjustment: NULL (no filter, default),
  "NSA" (Not Seasonally Adjusted), or "SA" (Seasonally Adjusted)

- fred_api_key:

  FRED API key (defaults to FRED_API_KEY environment variable)

## Value

A tibble with columns series_id, series_title, and optionally metadata
(a list column containing additional metadata when metadata = TRUE)

## Details

This function is a wrapper around
[[`fredr::fredr_series_search_text()`](https://rdrr.io/pkg/fredr/man/fredr_series_search.html)](https://sboysel.github.io/fredr/)
and requires you to have an [FRED API
key](https://fred.stlouisfed.org/docs/api/api_key.html).

## Examples

``` r
if (FALSE) {
find_fred("unemployment rate")

find_fred("GDP", max_results = 10)

find_fred("inflation", metadata = TRUE)

find_fred("employment population ratio", seasonality = "SA")
}
```
