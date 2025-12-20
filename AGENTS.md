# AGENTS.md

This file provides context about the `epidatatools` R package.

## Package Description

`epidatatools` is an R package containing various data tools used by the Economic Policy Institute.

## User-Facing Functions

Here is a list of the main user-facing functions available in this package:

- `averaged_quantile(x, w, probs, na.rm, quantiles_n, quantiles_w)`: Calculates the averaged (smoothed) quantile of a numeric vector.
- `averaged_median(x, w, na.rm, quantiles_n, quantiles_w)`: Calculates the averaged (smoothed) median of a numeric vector.
- `find_bls(search_string, max_results, metadata, survey, seasonality)`: Searches the BLS Data Finder for series matching a search string and returns series IDs and titles. Can optionally restrict search to a specific BLS survey and filter by seasonal adjustment.
- `find_fred(search_string, max_results, metadata, seasonality)`: Searches the FRED database for series matching a search string and returns series IDs and titles. Can optionally return additional metadata and filter by seasonal adjustment (NULL, "NSA", or "SA").
- `get_bea_nipa(tables, years, frequency, underlying, metadata, bea_api_key)`: Retrieves National Income and Product Accounts (NIPA) data from the Bureau of Economic Analysis (BEA) API. Requires a BEA API key. Supports both NIPA and NIUnderlyingDetail datasets. The `tables` parameter can be a named vector to add custom names (e.g., `c("gdp" = "T10101")`) which adds a "name" column as the first column. Returns a tibble with table_name, table_description, line_number, line_description, date_frequency, date, value, and date variables appropriate to the frequency of data returned (annual data includes only year; quarterly data includes year and quarter; monthly data includes year, quarter, and month). When metadata=TRUE, also includes unit_mult, metric_name, cl_unit, series_code, and note_text (list column containing all notes from the API response).
- `get_bea_regional(geo_fips, table_name, line_code, year, metadata, bea_api_key)`: Retrieves regional economic data from the Bureau of Economic Analysis (BEA) API. Requires a BEA API key. Takes a single table_name and line_code (or "ALL" for all line codes), but allows multiple geo_fips and years. A single geo_fips can be a state abbreviation (e.g., "NY"). Returns a tibble with geo_fips, geo_name, table_name, table_description, line_number, line_description, date_frequency, date, year, value. When metadata=TRUE, also includes cl_unit, mult_unit, and note_text (list column containing all notes from the API response). Results are sorted by geo_fips, table_name, line_number, date.
- `get_bls(series, start, end, metadata)`: Retrieves data from the Bureau of Labor Statistics (BLS) API. Requires a BLS API key.
- `crosstab(data, ..., w, percent)`: Creates a cross-tabulation of one or two variables in a data frame.
- `get_fred(series, start, end, metadata)`: Retrieves data from the Federal Reserve Economic Data (FRED) API. Requires a FRED API key.
- `interpolated_quantile(x, bin_size, probs, w, na.rm)`: Calculates the binned interpolated quantile of a numeric vector.
- `interpolated_median(x, bin_size, w, na.rm)`: Calculates the binned interpolated median of a numeric vector.
- `binipolate(data, x, probs, bin_size, .by, w)`: Summarizes a data frame as binned interpolated percentiles.
- `dl_ipums_micro(extract)`: Downloads a selection of IPUMS microdata extracts. Requires an IPUMS API key.
- `dl_ipums_acs1(years, variables, description, ...)`: Downloads IPUMS ACS 1-year files.
- `dl_ipums_asec(years, variables, description, ...)`: Downloads IPUMS CPS ASEC files.
- `dl_ipums_cps(months, variables, description, ...)`: Downloads IPUMS Monthly CPS files.
- `merge_status(x, y, ...)`: Joins two data frames and creates a `_merge` indicator column to show the merge status (left_only, right_only, both).
- `summarize_groups(.data, .groups, ...)`: Summarizes distinct groups in a data frame.

## Datasets

- `state_geocodes`: A data frame with 51 rows and 8 variables containing geographic codes for US states including FIPS codes, Census codes, and region/division classifications. Variables: state_name, state_abb, state_fips, state_census, division, division_name, region, region_name.

## Development Workflow

After any changes to the package (functions, documentation, or data), run the following commands to rebuild and verify:

```r
devtools::document()  # Regenerate documentation
devtools::check()     # Run R CMD check to verify package integrity
```

## Feature Development Workflow

When adding new functionality: (1) create an example that will verify the feature, (2) implement the feature, (3) verify the example works as intended.

## Debugging Workflow

When fixing issues: (1) create/run an example that demonstrates the problem, (2) implement the fix, (3) verify the example no longer exhibits the problem.

## Updates
Keep this document updated after modifying the functions in this package.
