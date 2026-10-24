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
