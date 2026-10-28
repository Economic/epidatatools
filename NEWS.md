# epidatatools (development version)

## Breaking changes

* `get_bls()` now requires the full argument names for `annual_average`,
  `metadata`, and `bls_api_key`. Only `series`, `start`, and `end` remain
  positional. `...` is reserved for future use; extra or misspelled arguments
  raise an error. Update positional calls to use, for example,
  `get_bls(id, 2024, 2025, metadata = TRUE, bls_api_key = key)`.

## Improvements

* `get_bls(annual_average = TRUE)` returns only BLS-published annual averages,
  dated January 1; missing averages warn (#44).
* Empty BLS responses now produce typed, zero-row tibbles, including nested
  `data` with `metadata = TRUE`.
* `get_bls()` warns about missing requested series/year pairs, including partial
  annual coverage, with a structured `bls_years_unavailable` warning (also
  `bls_annual_average_unavailable` for annual averages). It replaces BLS's no-data
  messages and can be suppressed selectively. Requests ending in future years
  return available data; entirely future intervals fail before API calls (#46).

# epidatatools 2.1.0

* `get_bls()` now explains missing values, such as October 2025 during the 2025 lapse
  in appropriations, in a warning of class `bls_missing_values` that gives BLS's
  footnote. Turn it off with `options(epidatatools.warn_missing = FALSE)` (#13).
* With `get_bls(metadata = TRUE)`, `value` in each series' nested `data` is now numeric
  (it was text, with `"-"` for missing values), and a new `footnotes` column gives BLS's
  footnotes.
* `get_bls()` errors and messages no longer show your BLS API key, which BLS includes
  in its daily-limit message.

# epidatatools 2.0.0

## Breaking changes

* `find_bls()` now searches a weekly catalog of every BLS series (8.3 million series
  from 33 programs), built by [bls-index](https://github.com/Economic/bls-index), instead
  of scraping the BLS Data Finder (#29).
  * The first search downloads the catalog (about 33 MB) into a cache, and later
    searches check for a newer one at most once a week. `refresh = TRUE` checks now.
  * Searching needs the arrow package, which `find_bls()` offers to install.

## Deprecations

* `dl_ipums_micro()`, `dl_ipums_acs1()`, `dl_ipums_asec()` and `dl_ipums_cps()` are
  renamed `get_ipums_micro()`, `get_ipums_acs1()`, `get_ipums_asec()` and
  `get_ipums_cps()`, to match the package's other data functions (#30). The old names
  still work, but are deprecated: calling one directly warns, and they will be removed
  in a future release.
* `interpolated_quantile()`, `interpolated_median()` and `binipolate()` are
  deprecated. Use `averaged_quantile()` and `averaged_median()` instead.

## Improvements

* `get_bls()` now returns data even when BLS fails to return series catalog metadata,
  which used to make the whole request fail. Affected series get an `NA`
  `series_title`, and a `bls_catalog_unavailable` warning lists them (#23).

# epidatatools 1.0.5
* Fix bug in `get_bls()` that duplicated rows when the BLS API call retrieves sublists of footnotes.

# epidatatools 1.0.4
* Fix `date_frequency` bug in `get_bls()` causing failures on 20+year API requests.

# epidatatools 1.0.3
* Fix bug with multiple same table IDs used in `get_bea_nipa()`

# epidatatools 1.0.2
* Fix `get_bls()` errors when catalog data is unavailable

# epidatatools 1.0.1
* Improve BEA API retrieval performance

# epidatatools 1.0.0
* Better error handling for BEA APIs
* correct bug in `get_bls()` producing duplicate output

# epidatatools 0.9.0
* New BEA API helper: `get_bea_regional()`
* Allow different API key names for BLS, FRED, and BEA fetchers.

# epidatatools 0.8.0
* New BEA API helper: `get_bea_nipa()`
* search FRED and BLS series with `find_fred()` and `find_bls()`

# epidatatools 0.7.1
* force `get_bls()` to produce numeric values

# epidatatools 0.7.0
* added `get_bls()` and `get_fred()` for retrieval from BLS and FRED APIs

# epidatatools 0.6.1
* corrected error in `dl_ipums_asec()` resulting in no downloaded data 

# epidatatools 0.6.0
* added passthrough options `...` to IPUMS microdata extract download helpers `dl_ipums_acs1()`, `dl_ipums_asec()`, `dl_ipums_cps()`

# epidatatools 0.5.0
* added averaged quantile functions: `averaged_quantile()`, `averaged_median()`

# epidatatools 0.4.0
* added interpolated binned quantile functions: `binipolate()`, `interpolated_quantile()`, `interpolated_median()`
* added IPUMS microdata extract download helpers: `dl_ipums_micro()`, `dl_ipums_acs1()`, `dl_ipums_asec()`, `dl_ipums_cps()`

# epidatatools 0.3.0
* initial pre-release release
