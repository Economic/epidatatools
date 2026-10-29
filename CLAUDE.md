# AGENTS.md

This file provides context about the `epidatatools` R package.

Read `AGENTS.md` for branch and roborev review guidance. `.roborev.toml` configures
automatic commit reviews and milestone panels, with a focus on concrete user
benefit and proportionate fixes.

## Package Description

`epidatatools` is an R package containing various data tools used by the Economic Policy Institute.

## User-Facing Functions

Here is a list of the main user-facing functions available in this package:

- `averaged_quantile(x, w, probs, na.rm, quantiles_n, quantiles_w)`: Calculates the averaged (smoothed) quantile of a numeric vector.
- `averaged_median(x, w, na.rm, quantiles_n, quantiles_w)`: Calculates the averaged (smoothed) median of a numeric vector.
- `find_bls(search_string, survey, seasonality, limit, titles_only, refresh)`: Searches the weekly bls-index catalog of every BLS series (https://github.com/Economic/bls-index) and returns series_id, series_title, program, survey_name, seasonal, begin_year, end_year and end_period. Every search word must match the title, survey name or a survey alias, by English stem. `survey` and `seasonality` filter by one or more values; `limit` (default 1000, `Inf` for all) caps the rows. The catalog is cached in `tools::R_user_dir("epidatatools", "cache")` and checked for updates weekly (`refresh = TRUE` checks now). Needs the arrow package (in Suggests). The code is a port of bls-index's reference client (`examples/find_bls.R`); its search rules live in bls-index's design.md. The y/ies stemming rule follows the reference client: only these endings drop a final stem `i`, long roots require `y` or `i` next, and short roots match whole words ending in `y`, `ie`, or `ies`. Tests use a fixture catalog in `tests/testthat/fixtures/`.
- `find_fred(search_string, max_results, metadata, seasonality)`: Searches the FRED database for series matching a search string and returns series IDs and titles. Can optionally return additional metadata and filter by seasonal adjustment (NULL, "NSA", or "SA").
- `get_bea_nipa(tables, years, frequency, underlying, metadata, bea_api_key)`: Retrieves National Income and Product Accounts (NIPA) data from the Bureau of Economic Analysis (BEA) API. Requires a BEA API key. Supports both NIPA and NIUnderlyingDetail datasets. The `tables` parameter can be a named vector to add custom names (e.g., `c("gdp" = "T10101")`) which adds a "name" column as the first column. Returns a tibble with table_name, table_description, line_number, line_description, date_frequency, date, value, and date variables appropriate to the frequency of data returned (annual data includes only year; quarterly data includes year and quarter; monthly data includes year, quarter, and month). When metadata=TRUE, also includes unit_mult, metric_name, cl_unit, series_code, and note_text (list column containing all notes from the API response).
- `get_bea_regional(geo_fips, table_name, line_code, year, metadata, bea_api_key)`: Retrieves regional economic data from the Bureau of Economic Analysis (BEA) API. Requires a BEA API key. Takes a single table_name and line_code (or "ALL" for all line codes), but allows multiple geo_fips and years. A single geo_fips can be a state abbreviation (e.g., "NY"). Returns a tibble with geo_fips, geo_name, table_name, table_description, line_number, line_description, date_frequency, date, year, value. When metadata=TRUE, also includes cl_unit, mult_unit, and note_text (list column containing all notes from the API response). Results are sorted by geo_fips, table_name, line_number, date.
- `get_bls(series, start, end, ..., annual_average, metadata, bls_api_key)`: Retrieves data from the Bureau of Labor Statistics (BLS) API. Requires a BLS API key. Calls the BLS API directly, splitting requests into batches of 50 series and spans of 20 years. Data and series catalogs (the source of `series_title` and metadata) are requested separately; if BLS fails to return a catalog, the data is still returned with an `NA` title and a `bls_catalog_unavailable` warning. API failures raise errors of class `bls_api_error`. Values that are not numbers (BLS sends "-" for missing data, such as October 2025) become `NA`: with a BLS footnote, one `bls_missing_values` warning gives the footnote text (off with `options(epidatatools.warn_missing = FALSE)`); without one, a `bls_unexpected_values` warning shows the raw values. With `metadata = TRUE`, each series' data has numeric values and a `footnotes` column. `annual_average = TRUE` requests and returns only BLS-published annual averages (M13/Q05/S03), with `date_frequency = "year"` and January 1 dates, including nested metadata output. No local averages are computed; series without published averages in the requested range have zero rows and a `bls_annual_average_unavailable` warning. Native A01 observations are not treated as computed averages. Default requests keep their usual frequency. When BLS returns no observations, flat output has zero rows with typed columns; nested output retains the series and metadata with a typed, zero-row data tibble. This also applies with `annual_average = FALSE`.
- `crosstab(data, ..., w, percent)`: Creates a cross-tabulation of one or two variables in a data frame.
- `get_fred(series, start, end, metadata)`: Retrieves data from the Federal Reserve Economic Data (FRED) API. Requires a FRED API key.
- `interpolated_quantile(x, bin_size, probs, w, na.rm)`: Calculates the binned interpolated quantile of a numeric vector. Deprecated in 2.0.0 (`lifecycle::deprecate_soft()`) in favor of `averaged_quantile()`.
- `interpolated_median(x, bin_size, w, na.rm)`: Calculates the binned interpolated median of a numeric vector. Deprecated in 2.0.0 in favor of `averaged_median()`.
- `binipolate(data, x, probs, bin_size, .by, w)`: Summarizes a data frame as binned interpolated percentiles. Deprecated in 2.0.0 in favor of `averaged_quantile()` inside `dplyr::reframe()`. All three deprecated functions call the internal `interpolated_quantile_impl()`, so each call warns once.
- `get_ipums_micro(extract)`: Retrieves a selection of IPUMS microdata extracts and returns them as a tibble. Requires an IPUMS API key.
- `get_ipums_acs1(years, variables, description, ...)`: Retrieves IPUMS ACS 1-year files.
- `get_ipums_asec(years, variables, description, ...)`: Retrieves IPUMS CPS ASEC files.
- `get_ipums_cps(months, variables, description, ...)`: Retrieves IPUMS Monthly CPS files.
- `dl_ipums_micro()`, `dl_ipums_acs1()`, `dl_ipums_asec()` and `dl_ipums_cps()`: deprecated in 2.0.0 (`lifecycle::deprecate_soft()`), and kept as wrappers that call the `get_ipums_*()` functions. Their shared help page (`ipums_micro_deprecated`) is listed with the other deprecated functions in the "Deprecated" section of `_pkgdown.yml`.
- `merge_status(x, y, ...)`: Joins two data frames and creates a `_merge` indicator column to show the merge status (left_only, right_only, both).
- `summarize_groups(.data, .groups, ...)`: Summarizes distinct groups in a data frame.

## Datasets

- `state_geocodes`: A data frame with 51 rows and 8 variables containing geographic codes for US states including FIPS codes, Census codes, and region/division classifications. Variables: state_name, state_abb, state_fips, state_census, division, division_name, region, region_name.

## Development Workflow

Every change to any file in this package — no matter how small — requires running the full development workflow below before considering the task complete.

After any changes to this package, run the following commands to rebuild and verify:

```r
devtools::document()  # Regenerate documentation
devtools::test()      # Run testthat tests
devtools::check()     # Run R CMD check to verify package integrity
pkgdown::check_pkgdown() # Check the pkgdown documentation site
```

Any change to the package requires re-running these commands, even if it seems like the verification goal has been narrowed. All of these commands must be run after any set of changes to the package contents, including changes to NEWS.md or other non-code files.

### API Tests

Tests for API functions (`get_bls()`, `get_bea_nipa()`, `get_bea_regional()`) require API keys to be set in environment variables:
- `BLS_API_KEY` for BLS functions
- `BEA_API_KEY` for BEA functions

Live API tests run only when `EPIDATATOOLS_LIVE_TESTS=true` and the key is set (see `tests/testthat/helper-live-api.R`), so a local `devtools::test()` or `devtools::check()` makes no API requests by default. CI sets it for its one R CMD check job with API keys. Run them locally with `Sys.setenv(EPIDATATOOLS_LIVE_TESTS = "true")`. A live test skips when the API is unavailable: BLS's daily limit of 500 requests (`REQUEST_NOT_PROCESSED`), or HTTP 429 or 5xx. Each `get_bls()` call makes 2 BLS requests (data and catalog).

Examples for `get_bls()`, `get_fred()`, `get_bea_*()` and `find_bls()` run only while the pkgdown site is built (`@examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")`), not in `R CMD check`. The pkgdown workflow fails a release build if any reference page has an example error, and warns on pull requests.

### Key Test File

- `test-find-bls-fixes.R`: Offline stemming regressions and cache recovery. The
  state writer reports success only after writing and renaming; the previous
  catalog is removed only after the replacement catalog and state are saved.
  Tests simulate state-save failure directly, preserving the old cache for later
  offline calls while allowing the current search to use the verified new catalog.
- `test-api-no-duplicates.R`: Verifies that API functions do not return duplicate data rows. This was added to catch regressions of a bug where multiple series queries returned duplicated observations.
- `test-bls-missing-values.R`: Tests `get_bls()` missing-value warnings and footnotes with a mocked `get_bls_api()`.
- `test-bls-annual-average.R`: Tests annual-only retrieval, M13/Q05/S03 dating, metadata/footnotes, missing annual averages, argument validation, request batching, and the BLS payload option offline.
- `test-bls-internals.R`: Tests `get_bls()` internals with a mocked `get_bls_api()` (no API calls), including request splitting, the catalog-failure fallback, and typed empty flat/nested output in default calls.

`get_bls()` coverage diagnostics compare every requested series/year pair with
observations after combining batches and selecting annual averages. One
`bls_years_unavailable` warning has `series_ids` and a `missing_years` tibble
(`series_id`, integer `year`); annual requests also retain the
`bls_annual_average_unavailable` class. Empty series share this warning. `NA`
observations count as present. Matching BLS no-data messages are replaced by the
warning; unrelated notes remain messages. Retrieval stops at `bls_current_year()`
while coverage retains the requested end; entirely future intervals raise
`bls_future_interval` before API calls. Catalog lookups use the accepted start
year. `test-bls-year-coverage.R` fixes the current year and verifies these paths
offline, including request counts, suppression, batches, and flat/nested output.

The public signature is `get_bls(series, start, end, ..., annual_average = FALSE,
metadata = FALSE, bls_api_key = Sys.getenv("BLS_API_KEY"))`. Optional arguments
require their full names; `rlang::check_dots_empty()` rejects positional options,
partial names, and unused arguments before API calls. This is an intentional
breaking change, tested in `test-bls-annual-average.R`.

## Feature Development Workflow

When adding new functionality: (1) create an example that will verify the feature, (2) implement the feature, (3) verify the example works as intended.

## Debugging Workflow

When fixing issues: (1) create/run an example that demonstrates the problem, (2) implement the fix, (3) verify the example no longer exhibits the problem.

## Updates
Keep this document updated after modifying the functions in this package.
