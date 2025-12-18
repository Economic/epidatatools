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
if (FALSE) { # nzchar(Sys.getenv("FRED_API_KEY"))
get_fred("UNRATE")

series = c(
  gdp = "GDP",
  urate = "UNRATE"
)
get_fred(series, start = as.Date("2024-07-01"), end = 2024)

complete_results = get_fred(series, start = 2020, end = 2025, metadata = TRUE)
complete_results

complete_results |>
  tidyr::unnest(metadata)
}
```
