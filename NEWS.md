# epidatatools (development version)

## Breaking changes

* `find_bls()` now searches a weekly catalog of every BLS series (8.3 million series
  from 33 programs), built by [bls-index](https://github.com/Economic/bls-index), instead
  of scraping the BLS Data Finder. Its arguments are now `search_string`, `survey`,
  `seasonality`, `limit`, `titles_only` and `refresh`:
  * `max_results` is gone. Use `limit`, which defaults to 1,000.
  * `metadata` is gone. Results always include `series_id`, `series_title`, `program`,
    `survey_name`, `seasonal`, `begin_year`, `end_year` and `end_period`.
  * `survey` and `seasonality` take several values, such as `survey = c("la", "ln")`.
  * The first search downloads the catalog (about 33 MB) into a cache, and later
    searches check for a newer one at most once a week. `refresh = TRUE` checks now.
  * Searching needs the arrow package, which `find_bls()` offers to install.
* rvest is no longer a dependency.

## Deprecations

* `dl_ipums_micro()`, `dl_ipums_acs1()`, `dl_ipums_asec()` and `dl_ipums_cps()` are
  renamed `get_ipums_micro()`, `get_ipums_acs1()`, `get_ipums_asec()` and
  `get_ipums_cps()`, to match the package's other data functions (#30). The old names
  still work, but are deprecated: calling one directly warns, and they will be removed
  in a future release.

# epidatatools 1.1.0
* Fix BEA API error handling so errors returned under `Results` (e.g., from `GetData` calls) are reported instead of failing later with an unrelated message (#22).
* `get_bls()` now returns data even when BLS fails to return series catalog metadata, which used to make the whole request fail. Affected series get an `NA` `series_title`, and a `bls_catalog_unavailable` warning lists them (#23).
* `get_bls()` now calls the BLS API directly instead of through the blsR package, which is no longer a dependency. Results are unchanged.
* Fix BEA and BLS functions on R versions before 4.4, which lack base R's `%||%` operator; it is now imported from rlang.

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
