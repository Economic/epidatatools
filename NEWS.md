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
