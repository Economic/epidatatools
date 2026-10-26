# Deprecated IPUMS download functions

**\[deprecated\]**

These functions were renamed in epidatatools 2.0.0 to match the
package's other data functions, such as
[`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
and
[`get_fred()`](https://economic.github.io/epidatatools/reference/get_fred.md):

- `dl_ipums_micro()` is now
  [`get_ipums_micro()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)

- `dl_ipums_acs1()` is now
  [`get_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)

- `dl_ipums_asec()` is now
  [`get_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)

- `dl_ipums_cps()` is now
  [`get_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)

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
