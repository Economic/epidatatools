# Changelog

## epidatatools 2.0.0

### Breaking changes

- [`find_bls()`](https://economic.github.io/epidatatools/reference/find_bls.md)
  now searches a weekly catalog of every BLS series (8.3 million series
  from 33 programs), built by
  [bls-index](https://github.com/Economic/bls-index), instead of
  scraping the BLS Data Finder
  ([\#29](https://github.com/Economic/epidatatools/issues/29)).
  - The first search downloads the catalog (about 33 MB) into a cache,
    and later searches check for a newer one at most once a week.
    `refresh = TRUE` checks now.
  - Searching needs the arrow package, which
    [`find_bls()`](https://economic.github.io/epidatatools/reference/find_bls.md)
    offers to install.

### Deprecations

- [`dl_ipums_micro()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md)
  and
  [`dl_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md)
  are renamed
  [`get_ipums_micro()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`get_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  [`get_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)
  and
  [`get_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro.md),
  to match the package’s other data functions
  ([\#30](https://github.com/Economic/epidatatools/issues/30)). The old
  names still work, but are deprecated: calling one directly warns, and
  they will be removed in a future release.
- [`interpolated_quantile()`](https://economic.github.io/epidatatools/reference/interpolated_quantile.md),
  [`interpolated_median()`](https://economic.github.io/epidatatools/reference/interpolated_median.md)
  and
  [`binipolate()`](https://economic.github.io/epidatatools/reference/binipolate.md)
  are deprecated. Use
  [`averaged_quantile()`](https://economic.github.io/epidatatools/reference/averaged_quantile.md)
  and
  [`averaged_median()`](https://economic.github.io/epidatatools/reference/averaged_median.md)
  instead.

### Improvements

- [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  now returns data even when BLS fails to return series catalog
  metadata, which used to make the whole request fail. Affected series
  get an `NA` `series_title`, and a `bls_catalog_unavailable` warning
  lists them
  ([\#23](https://github.com/Economic/epidatatools/issues/23)).

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
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md)
  resulting in no downloaded data

## epidatatools 0.6.0

- added passthrough options `...` to IPUMS microdata extract download
  helpers
  [`dl_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md)

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
  [`dl_ipums_micro()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md),
  [`dl_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro_deprecated.md)

## epidatatools 0.3.0

- initial pre-release release
