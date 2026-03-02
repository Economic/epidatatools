# Implementation Plan: `get_bea_nipa` Performance Bottlenecks

This plan implements the fixes described in `get_bea_nipa_bottlenecks.md` in three commits. Each commit is independently testable and leaves the package in a working state.

## Commit 1: Add unit tests for date helpers

**Why first:** These tests lock in the current behavior of `parse_bea_date` and `extract_period_component` before we rewrite them. They serve as a regression safety net for Commits 2 and 3.

### Step 1.1: Create `tests/testthat/test-date-helpers.R`

```r
test_that("parse_bea_date parses quarterly periods", {
  result = parse_bea_date(c("2024Q1", "2024Q4"))
  expect_equal(result, as.Date(c("2024-01-01", "2024-10-01")))
})

test_that("parse_bea_date parses monthly periods", {
  result = parse_bea_date(c("2024M01", "2024M12"))
  expect_equal(result, as.Date(c("2024-01-01", "2024-12-01")))
})

test_that("parse_bea_date parses annual periods", {
  result = parse_bea_date(c("2024", "2020"))
  expect_equal(result, as.Date(c("2024-01-01", "2020-01-01")))
})

test_that("parse_bea_date handles mixed frequencies", {
  result = parse_bea_date(c("2024Q1", "2024", "2024M06"))
  expect_equal(result, as.Date(c("2024-01-01", "2024-01-01", "2024-06-01")))
})

test_that("parse_bea_date returns NA for unrecognized formats", {
  result = parse_bea_date(c("garbage", "2024Q1"))
  expect_true(is.na(result[1]))
  expect_equal(result[2], as.Date("2024-01-01"))
})

test_that("parse_bea_date handles empty input", {
  result = parse_bea_date(character(0))
  expect_equal(result, as.Date(character(0)))
})

test_that("extract_period_component extracts year", {
  result = extract_period_component(c("2024Q1", "2024", "2024M06"), "year")
  expect_equal(result, c(2024L, 2024L, 2024L))
})

test_that("extract_period_component extracts quarter", {
  result = extract_period_component(c("2024Q1", "2024Q4", "2024"), "quarter")
  expect_equal(result, c(1L, 4L, NA_integer_))
})

test_that("extract_period_component extracts month", {
  result = extract_period_component(c("2024M01", "2024M12", "2024Q1"), "month")
  expect_equal(result, c(1L, 12L, NA_integer_))
})

test_that("extract_period_component returns NA for unrecognized formats", {
  result = extract_period_component(c("garbage"), "year")
  expect_equal(result, NA_integer_)
})

test_that("extract_period_component handles empty input", {
  result = extract_period_component(character(0), "year")
  expect_equal(result, integer(0))
})

test_that("normalize_api_frequency classifies BEA periods", {
  result = normalize_api_frequency(c("2024Q1", "2024M06", "2024"), "bea")
  expect_equal(result, c("quarter", "month", "year"))
})
```

Note: `parse_bea_date`, `extract_period_component`, and `normalize_api_frequency` are internal (not exported), so the test file needs to access them. `testthat` automatically sources files in `R/` during `devtools::test()`, so internal functions are available without `:::`.

### Step 1.2: Verify

```r
devtools::test(filter = "date-helpers")
```

All tests must pass against the **current** (unmodified) implementations before proceeding.

---

## Commit 2: Vectorize date helpers (Bottlenecks 3 and 4)

**Files modified:** `R/date_helpers.R` only.

These functions are already called on vectors by `process_nipa_dates_and_values` (via `dplyr::mutate`), so the vectorized versions are drop-in replacements. No callers need to change.

### Step 2.1: Replace `parse_bea_date` (lines 97-116 of `R/date_helpers.R`)

Replace the entire function body. The current implementation uses `purrr::map` + `unlist` + `as.Date(origin = ...)`. The new implementation uses subset-and-reassemble with vectorized `grepl`, `lubridate::yq`, `lubridate::ym`, and `as.Date`.

```r
parse_bea_date = function(time_period) {
  result = rep(as.Date(NA), length(time_period))

  is_quarterly = grepl("Q[1-4]$", time_period)
  is_monthly = grepl("M(0[1-9]|1[0-2])$", time_period)
  is_annual = grepl("^[0-9]{4}$", time_period)

  if (any(is_quarterly)) {
    result[is_quarterly] =
      lubridate::yq(time_period[is_quarterly]) |> as.Date()
  }

  if (any(is_monthly)) {
    tp = time_period[is_monthly]
    result[is_monthly] =
      lubridate::ym(paste0(substr(tp, 1, 4), "-", substr(tp, 6, 7))) |> as.Date()
  }

  if (any(is_annual)) {
    result[is_annual] =
      as.Date(paste0(time_period[is_annual], "-01-01"))
  }

  result
}
```

### Step 2.2: Replace `extract_period_component` (lines 128-164 of `R/date_helpers.R`)

Replace the entire function body. The current implementation uses `purrr::map_int` with per-element `grepl` + `substr`. The new implementation uses a single vectorized `grepl` call to build a mask, then vectorized `substr` + `as.integer` on the matching subset.

```r
extract_period_component = function(
  period,
  component = c("year", "quarter", "month")
) {
  component = match.arg(component)
  result = rep(NA_integer_, length(period))

  switch(
    component,
    year = {
      mask = grepl("^[0-9]{4}", period)
      result[mask] = as.integer(substr(period[mask], 1, 4))
    },
    quarter = {
      mask = grepl("Q[1-4]$", period)
      result[mask] = as.integer(substr(period[mask], 6, 6))
    },
    month = {
      mask = grepl("M(0[1-9]|1[0-2])$", period)
      result[mask] = as.integer(substr(period[mask], 6, 7))
    }
  )

  result
}
```

### Step 2.3: Verify

```r
devtools::test()    # All tests must pass (date-helpers + api-no-duplicates)
devtools::check()   # R CMD check must pass
```

The API tests (if API keys are available) provide end-to-end verification that the vectorized helpers produce the same output as the originals.

---

## Commit 3: Vectorize JSON parsing and tibble construction (Bottlenecks 1 and 2)

**Files modified:** `R/bea.R` only.

This commit changes `simplifyVector` to `TRUE` and simultaneously replaces all row-by-row tibble construction with vectorized column construction. These must be a single atomic commit — neither change works without the other.

### Step 3.1: Change `simplifyVector` (line 88 of `R/bea.R`)

```r
# Before:
result = jsonlite::fromJSON(content, simplifyVector = FALSE)

# After:
result = jsonlite::fromJSON(content, simplifyVector = TRUE)
```

### Step 3.2: Replace `extract_all_note_texts` (lines 102-107 of `R/bea.R`)

With `simplifyVector = TRUE`, `raw_notes` is a data frame (not a list of lists). Replace `purrr::map_chr` with direct column access. Add `is.data.frame()` as a defensive guard.

```r
extract_all_note_texts = function(raw_notes) {
  if (is.null(raw_notes) || !is.data.frame(raw_notes) || nrow(raw_notes) == 0) {
    return(character(0))
  }
  raw_notes$NoteText
}
```

### Step 3.3: Replace `extract_table_description` (lines 114-126 of `R/bea.R`)

The `for (note in raw_notes)` loop would iterate over columns of a data frame. Replace with vectorized `which()` lookup. Add `is.data.frame()` guard.

```r
extract_table_description = function(raw_notes, table_name) {
  if (is.null(raw_notes) || !is.data.frame(raw_notes) || nrow(raw_notes) == 0) {
    return(NA_character_)
  }

  match_idx = which(raw_notes$NoteRef == table_name)
  if (length(match_idx) == 0) {
    return(NA_character_)
  }

  raw_notes$NoteText[match_idx[1]]
}
```

### Step 3.4: Add defensive comment to `handle_bea_api_error` (line 7 of `R/bea.R`)

No functional change, but add a comment documenting the latent risk with `ErrorDetail`.

```r
handle_bea_api_error = function(result) {
  # Check for API errors
  if (!is.null(result$BEAAPI$Error)) {
    error_info = result$BEAAPI$Error

    # Extract all error components
    error_description = error_info$APIErrorDescription %||% "Unknown error"
    error_code = error_info$APIErrorCode %||% NA_character_
    # Note: ErrorDetail is assumed to be a single JSON object (not an array).
    # If the BEA API ever returns it as an array, simplifyVector = TRUE would
    # convert it to a data frame and the is.na() check below would fail.
    error_detail = error_info$ErrorDetail$Description %||% NA_character_

    # ... rest unchanged ...
```

### Step 3.5: Replace `fetch_bea_nipa_complete` tibble construction (lines 342-346 of `R/bea.R`)

Replace the `purrr::map_dfr` call with direct vectorized tibble construction from data frame columns. `raw_data` is now a data frame (thanks to `simplifyVector = TRUE`), so `raw_data$TableName` etc. are character vectors.

```r
  # Before:
  data_tibble = purrr::map_dfr(
    raw_data,
    ~ convert_nipa_row_to_tibble(.x, table_description, all_note_texts)
  )

  # After:
  data_tibble = tibble::tibble(
    table_name       = raw_data$TableName,
    table_description = table_description,
    series_code      = raw_data$SeriesCode,
    line_number      = as.integer(raw_data$LineNumber),
    line_description = raw_data$LineDescription,
    time_period      = raw_data$TimePeriod,
    metric_name      = raw_data$METRIC_NAME,
    cl_unit          = raw_data$CL_UNIT,
    unit_mult        = as.integer(raw_data$UNIT_MULT),
    data_value       = raw_data$DataValue,
    note_ref         = raw_data$NoteRef,
    note_text        = list(all_note_texts)
  )
```

### Step 3.6: Replace `fetch_bea_regional` tibble construction (lines 552-579 of `R/bea.R`)

Replace the `purrr::map_dfr` block with vectorized construction. Inline the line-info extraction logic (replacing the per-row `extract_regional_line_info` call).

```r
  # Before:
  purrr::map_dfr(raw_data, function(row) {
    line_info = extract_regional_line_info(...)
    ...
  })

  # After:
  if (is.character(line_code) && toupper(line_code) == "ALL") {
    line_numbers = as.integer(sub(".*-", "", raw_data$Code))
    line_descriptions = raw_data$Description
  } else {
    line_numbers = as.integer(line_code)
    line_descriptions = statistic
  }

  tibble::tibble(
    geo_fips         = raw_data$GeoFips,
    geo_name         = raw_data$GeoName,
    table_name       = table_name,
    table_description = public_table,
    line_number      = line_numbers,
    line_description = line_descriptions,
    date_frequency   = normalize_api_frequency(raw_data$TimePeriod, "bea"),
    date             = parse_api_date(raw_data$TimePeriod),
    year             = extract_period_component(raw_data$TimePeriod, "year"),
    value            = parse_data_value(raw_data$DataValue),
    cl_unit          = raw_data$CL_UNIT,
    mult_unit        = raw_data$UNIT_MULT,
    note_text        = list(all_note_texts)
  )
```

### Step 3.7: Remove dead code from `R/bea.R`

Delete the following functions entirely:

1. `convert_nipa_row_to_tibble` (lines 136-157) — replaced by inline vectorized construction in Step 3.5
2. `extract_regional_line_info` (lines 175-197) — replaced by inline vectorized code in Step 3.6
3. `process_bea_series_groups` (lines 369-426) — never called anywhere in the codebase

### Step 3.8: Verify

```r
devtools::test()      # All tests must pass
devtools::check()     # R CMD check must pass
pkgdown::build_site() # Documentation must build
```

---

## Verification checklist

After all three commits:

- [ ] `devtools::test()` passes (both `test-date-helpers.R` and `test-api-no-duplicates.R`)
- [ ] `devtools::check()` passes with no errors or warnings
- [ ] `pkgdown::build_site()` builds successfully
- [ ] Manual smoke test: `get_bea_nipa("T10101", years = 2023:2024, frequency = "quarter")` returns expected data
- [ ] Manual smoke test: `get_bea_regional(geo_fips = "36000", table_name = "SAINC1", line_code = 1, year = 2022:2023)` returns expected data
- [ ] Manual smoke test: `get_bea_nipa("T10101", years = 2023, frequency = "year", metadata = TRUE)` includes metadata columns
- [ ] Manual smoke test: `get_bea_regional(geo_fips = "36000", table_name = "SAINC1", line_code = "ALL", year = 2023)` correctly extracts line numbers from codes

## Files modified

| File | Commit | Changes |
|---|---|---|
| `tests/testthat/test-date-helpers.R` | 1 | New file: unit tests for date helper functions |
| `R/date_helpers.R` | 2 | Rewrite `parse_bea_date` and `extract_period_component` |
| `R/bea.R` | 3 | Change `simplifyVector`, rewrite `extract_all_note_texts`, `extract_table_description`, `fetch_bea_nipa_complete`, `fetch_bea_regional`; add comment to `handle_bea_api_error`; remove `convert_nipa_row_to_tibble`, `extract_regional_line_info`, `process_bea_series_groups` |
