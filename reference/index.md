# Package index

## Quantiles

Calculate weighted percentiles.

- [`binipolate()`](https://economic.github.io/epidatatools/reference/binipolate.md)
  **\[superseded\]** : Summarize a data frame as binned interpolated
  percentiles
- [`averaged_median()`](https://economic.github.io/epidatatools/reference/averaged_median.md)
  : Calculate the averaged (smoothed) median
- [`averaged_quantile()`](https://economic.github.io/epidatatools/reference/averaged_quantile.md)
  : Calculate the averaged (smoothed) quantile
- [`interpolated_median()`](https://economic.github.io/epidatatools/reference/interpolated_median.md)
  **\[superseded\]** : Calculate the binned interpolated median
- [`interpolated_quantile()`](https://economic.github.io/epidatatools/reference/interpolated_quantile.md)
  **\[superseded\]** : Calculate the binned interpolated quantile

## Download data

Helpers to download data from specific APIs

### IPUMS microdata extracts

- [`dl_ipums_micro()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)
  [`dl_ipums_acs1()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)
  [`dl_ipums_asec()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)
  [`dl_ipums_cps()`](https://economic.github.io/epidatatools/reference/ipums_micro.md)
  : Download a selection of IPUMS microdata extracts

### BLS time series

- [`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md)
  : Retrieve data from the BLS API
- [`find_bls()`](https://economic.github.io/epidatatools/reference/find_bls.md)
  : Find BLS Series by Search String

### FRED time series

- [`get_fred()`](https://economic.github.io/epidatatools/reference/get_fred.md)
  : Retrieve data from the FRED API
- [`find_fred()`](https://economic.github.io/epidatatools/reference/find_fred.md)
  : Find FRED Series by Search String

### BEA data

- [`get_bea_nipa()`](https://economic.github.io/epidatatools/reference/get_bea_nipa.md)
  : Retrieve NIPA data from the BEA API
- [`get_bea_regional()`](https://economic.github.io/epidatatools/reference/get_bea_regional.md)
  : Retrieve Regional data from the BEA API

## Included datasets

- [`state_geocodes`](https://economic.github.io/epidatatools/reference/state_geocodes.md)
  : State Geographic Codes

## Other functions

- [`crosstab()`](https://economic.github.io/epidatatools/reference/crosstab.md)
  : Cross-tabulate one or two variables
- [`merge_status()`](https://economic.github.io/epidatatools/reference/merge_status.md)
  : Join data frames and create a merge indicator
- [`summarize_groups()`](https://economic.github.io/epidatatools/reference/summarize_groups.md)
  : Summarize distinct groups
