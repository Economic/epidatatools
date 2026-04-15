# Changelog

## epidatatools 1.0.5

- Fix bug in
  [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  that duplicated rows when the BLS API call retrieves sublists of
  footnotes.

## epidatatools 1.0.4

- Fix `date_frequency` bug in
  [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  causing failures on 20+year API requests.

## epidatatools 1.0.3

- Fix bug with multiple same table IDs used in
  [`get_bea_nipa()`](https://economic.github.io/epidatatools/reference/get_bea_nipa.md)

## epidatatools 1.0.2

- Fix
  [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  errors when catalog data is unavailable

## epidatatools 1.0.1

- Improve BEA API retrieval performance

## epidatatools 1.0.0

- Better error handling for BEA APIs
- correct bug in
  [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  producing duplicate output

## epidatatools 0.9.0

- New BEA API helper:
  [`get_bea_regional()`](https://economic.github.io/epidatatools/reference/get_bea_regional.md)
- Allow different API key names for BLS, FRED, and BEA fetchers.

## epidatatools 0.8.0

- New BEA API helper:
  [`get_bea_nipa()`](https://economic.github.io/epidatatools/reference/get_bea_nipa.md)
- search FRED and BLS series with
  [`find_fred()`](https://economic.github.io/epidatatools/reference/find_fred.md)
  and
  [`find_bls()`](https://economic.github.io/epidatatools/reference/find_bls.md)

## epidatatools 0.7.1

- force
  [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  to produce numeric values

## epidatatools 0.7.0

- added
  [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  and
  [`get_fred()`](https://economic.github.io/epidatatools/reference/get_fred.md)
  for retrieval from BLS and FRED APIs

## epidatatools 0.6.1

- corrected error in
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)
  resulting in no downloaded data

## epidatatools 0.6.0

- added passthrough options `...` to IPUMS microdata extract download
  helpers
  [`dl_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`dl_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)

## epidatatools 0.5.0

- added averaged quantile functions:
  [`averaged_quantile()`](https://economic.github.io/epidatatools/reference/averaged_quantile.md),
  [`averaged_median()`](https://economic.github.io/epidatatools/reference/averaged_median.md)

## epidatatools 0.4.0

- added interpolated binned quantile functions:
  [`binipolate()`](https://economic.github.io/epidatatools/reference/binipolate.md),
  [`interpolated_quantile()`](https://economic.github.io/epidatatools/reference/interpolated_quantile.md),
  [`interpolated_median()`](https://economic.github.io/epidatatools/reference/interpolated_median.md)
- added IPUMS microdata extract download helpers:
  [`dl_ipums_micro()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`dl_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`dl_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)

## epidatatools 0.3.0

- initial pre-release release
