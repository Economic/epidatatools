# Get a selection of IPUMS microdata extracts

Convenience functions for downloading samples and variables from
[IPUMS](https://www.ipums.org/) microdata using their
[API](https://developer.ipums.org/) and package
[`ipumsr`](https://tech.popdata.org/ipumsr/articles/ipums-api-micro.html).

## Usage

``` r
get_ipums_micro(extract)

get_ipums_acs1(years = NULL, variables, description = NULL, ...)

get_ipums_asec(years = NULL, variables, description = NULL, ...)

get_ipums_cps(months = NULL, variables, description = NULL, ...)
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

- `get_ipums_micro()`: base function group

- `get_ipums_acs1()`: Get IPUMS ACS 1-year files

- `get_ipums_asec()`: Get IPUMS CPS ASEC files

- `get_ipums_cps()`: Get IPUMS Monthly CPS files

## Examples

``` r
if (FALSE) {
# example ASEC download
get_ipums_asec(2021:2023, c("YEAR", "OFFPOV", "ASECWT"))

# example monthly CPS download
begin_month = lubridate::ym("2022 September")
end_month = lubridate::ym("2024 August")
cps_months = seq(begin_month, end_month, by = "month")
get_ipums_cps(cps_months, c("EARNWT", "HOURWAGE2"))

# use get_ipums_micro with a custom extract
extract = ipumsr::define_extract_micro(
  collection = "cps",
  description = "CPS ASEC extract",
  samples = c("cps2021_03s", "cps2022_03s", "cps2023_03s"),
  variables = c("YEAR", "OFFPOV", "ASECWT")
)
get_ipums_micro(extract)
}
```
