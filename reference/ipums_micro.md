# Download a selection of IPUMS microdata extracts

Convenience functions for downloading samples and variables from
[IPUMS](https://www.ipums.org/) microdata using their
[API](https://developer.ipums.org/) and package
[`ipumsr`](https://tech.popdata.org/ipumsr/articles/ipums-api-micro.html).

## Usage

``` r
dl_ipums_micro(extract)

dl_ipums_acs1(years = NULL, variables, description = NULL, ...)

dl_ipums_asec(years = NULL, variables, description = NULL, ...)

dl_ipums_cps(months = NULL, variables, description = NULL, ...)
```

## Arguments

- extract:

  an IPUMS microdata extract as defined by
  [`ipumsr::define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.html)

- years:

  a vector of years

- variables:

  a vector of variable names, or a list of detailed variable
  specifications as created by
  [`ipumsr::var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.html)

- description:

  description for the extract

- ...:

  arguments passed to
  [`ipumsr::define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.html)
  other than collection, description, samples, variables

- months:

  a vector of dates representing months of CPS samples.

## Value

a tibble of microdata from the IPUMS API

## Details

These functions are simply wrappers around
[`ipumsr`](https://tech.popdata.org/ipumsr/) and require you to have an
[IPUMS API
key](https://tech.popdata.org/ipumsr/articles/ipums-api.html#set-key)
saved in the `IPUMS_API_KEY` environment variable.

## Functions

- `dl_ipums_micro()`: base function group

- `dl_ipums_acs1()`: Download IPUMS ACS 1-year files

- `dl_ipums_asec()`: Download IPUMS CPS ASEC

- `dl_ipums_cps()`: Download IPUMS Monthly CPS

## Examples

``` r
if (FALSE) {
# example ASEC download
dl_ipums_asec(2021:2023, c("YEAR", "OFFPOV", "ASECWT"))

# example monthly CPS download
begin_month = lubridate::ym("2022 September")
end_month = lubridate::ym("2024 August")
cps_months = seq(begin_month, end_month, by = "month")
dl_ipums_cps(cps_months, c("EARNWT", "HOURWAGE2"))

# use dl_ipums_micro with a custom extract
extract = ipumsr::define_extract_micro(
  collection = "cps",
  description = "CPS ASEC extract",
  samples = c("cps2021_03s", "cps2022_03s", "cps2023_03s"),
  variables = c("YEAR", "OFFPOV", "ASECWT")
)
dl_ipums_micro(extract)
}
```
