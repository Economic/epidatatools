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
if (FALSE) { # nzchar(Sys.getenv("BLS_API_KEY"))
get_bls("LNU02300060", start = 2020, end = 2024)

bls_series_ids = c(
  emp_fb_2534 = "LNU02073399",
  epop_asianmen_2554 = "LNU02332330Q",
  cpi_semi = "CUUS0000SA0",
  "LNU02300060"
)
get_bls(bls_series_ids, start = 2024, end = 2024)

complete_results = get_bls(bls_series_ids, start = 2024, end = 2024, metadata = TRUE)
complete_results

complete_results |>
  tidyr::unnest(metadata)
}
```
