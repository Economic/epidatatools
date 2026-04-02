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
#> Error in purrr::map(seq_along(series), ~fetch_fred_series_complete(.x,     series, observation_start, observation_end)): ℹ In index: 1.
#> Caused by error:
#> ! 500: Internal Server Error

series = c(
  gdp = "GDP",
  urate = "UNRATE"
)
get_fred(series, start = as.Date("2024-07-01"), end = 2024)
#> Request failed [502]. Retrying in 1 seconds...
#> Error in purrr::map(seq_along(series), ~fetch_fred_series_complete(.x,     series, observation_start, observation_end)): ℹ In index: 1.
#> Caused by error:
#> ! 500: Internal Server Error

complete_results = get_fred(series, start = 2020, end = 2025, metadata = TRUE)
#> Error in purrr::map(seq_along(series), ~fetch_fred_series_complete(.x,     series, observation_start, observation_end)): ℹ In index: 1.
#> Caused by error:
#> ! 500: Internal Server Error
complete_results
#> Error: object 'complete_results' not found

complete_results |>
  tidyr::unnest(metadata)
#> Error: object 'complete_results' not found
```
