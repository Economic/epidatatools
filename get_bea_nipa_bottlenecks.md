# `get_bea_nipa` Performance Bottlenecks

## Context

`epidatatools::get_bea_nipa` is significantly slower than `bea.R::beaGet` for the same API query. The slowness is entirely in post-response data processing, not in the API call itself. The following bottlenecks were identified by reading the source code in `R/bea.R` and `R/date_helpers.R`.

Bottlenecks 1 and 2 are tightly coupled and should be implemented together. Bottlenecks 3 and 4 are independent of each other and of 1/2.

## Bottleneck 1: `simplifyVector = FALSE` in JSON parsing (High impact)

**Location:** `get_bea_api` in `R/bea.R`, line 88.

**Problem:** The JSON response is parsed with `simplifyVector = FALSE`:

```r
result = jsonlite::fromJSON(content, simplifyVector = FALSE)
```

This forces the `Data` array to remain as a nested list-of-lists, which necessitates the row-by-row processing in Bottleneck 2. With the default `simplifyVector = TRUE`, `jsonlite` automatically converts the `Data` array into a data frame.

**Fix:** Change to `simplifyVector = TRUE`:

```r
result = jsonlite::fromJSON(content, simplifyVector = TRUE)
```

**Affected functions:** This changes the structure returned by `get_bea_api` from lists-of-lists to data frames. The following functions must be adapted:

### 1a. `extract_all_note_texts` (line 102)

`raw_notes` becomes a data frame instead of a list-of-lists, so `purrr::map_chr` iteration no longer applies. Replace with direct column access:

```r
extract_all_note_texts = function(raw_notes) {
  if (is.null(raw_notes) || !is.data.frame(raw_notes) || nrow(raw_notes) == 0) {
    return(character(0))
  }
  raw_notes$NoteText
}
```

Note: the guard changes from `length(raw_notes) == 0` to `nrow(raw_notes) == 0` because `length()` on a data frame returns the number of columns, not rows. The `is.data.frame()` check is a defensive guard: the BEA API consistently returns `Notes` as a JSON array (producing a data frame with `simplifyVector = TRUE`), but if it were ever returned as a bare JSON object, jsonlite would produce a named list instead. `nrow()` on a list returns `NULL`, and `NULL == 0` yields `logical(0)`, which would crash the `||` check. The `is.data.frame()` guard prevents this.

### 1b. `extract_table_description` (line 114)

The `for (note in raw_notes)` loop iterates over columns of a data frame, not rows. Replace with vectorized lookup:

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

### 1c. `handle_bea_api_error` (line 7)

The error response is a JSON object (not an array), so `simplifyVector` does not change its structure. No changes needed, but the function should be tested against a real error response to confirm.

**Latent risk:** If the BEA API ever returns `ErrorDetail` as a JSON array (rather than a single object), `simplifyVector = TRUE` would convert it to a data frame. Then `error_info$ErrorDetail$Description` would return a character vector, and the subsequent `if (!is.na(error_detail))` check would throw R's "the condition has length > 1" error. This does not appear to happen with the current API, but a defensive comment should be added to the function.

### 1d. `parse_data_value` (line 132)

Currently uses `%||%` for null-coalescing on a single element. With `simplifyVector = TRUE`, the input becomes a character vector where missing values are `NA` (not `NULL`). Both `gsub` and `as.numeric` are vectorized and handle `NA` correctly, so `parse_data_value` works as-is on vectors. The `%||%` becomes a no-op (the vector is never `NULL`), which is harmless.

## Bottleneck 2: Row-by-row tibble construction (High impact)

**Location:** `fetch_bea_nipa_complete` in `R/bea.R`, lines 343-346 and `fetch_bea_regional` in `R/bea.R`, lines 553-579.

**Problem:** Both functions create a separate 1-row tibble for every row in the API response via `purrr::map_dfr`, then row-bind them all together. For large tables (e.g., T10705 with `Year = "X"`), the API returns thousands of rows. Each iteration allocates a tibble, and `map_dfr` progressively binds them. This is the classic "growing a data frame in a loop" anti-pattern.

With Bottleneck 1 fixed (`simplifyVector = TRUE`), `raw_data` is already a data frame and we can construct the output tibble directly from column vectors.

> **Critical: Bottlenecks 1 and 2 must be applied as a single atomic commit.** If Bottleneck 1 is applied alone (without Bottleneck 2), `purrr::map_dfr(raw_data, ...)` would iterate over **columns** of the data frame instead of rows, silently producing corrupt output. If Bottleneck 2 is applied alone (without Bottleneck 1), `raw_data$TableName` etc. would be `NULL` because `raw_data` is still a list of lists. Neither bottleneck is safe to deploy independently.

### 2a. Fix for `fetch_bea_nipa_complete` (lines 342-346)

Replace:

```r
data_tibble = purrr::map_dfr(
  raw_data,
  ~ convert_nipa_row_to_tibble(.x, table_description, all_note_texts)
)
```

With:

```r
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

`table_description` is a scalar string. The `tibble()` constructor recycles scalars to match the length of the other columns, so no explicit `rep()` is needed.

`note_text = list(all_note_texts)` wraps the full note vector in a list column where every row shares the same value. This preserves the current behavior. (Whether notes should instead be matched per-row via `note_ref` is a separate design question.)

With `simplifyVector = TRUE`, jsonlite converts missing JSON values to `NA` in data frame columns. The `%||%` null-coalescing used in the current `convert_nipa_row_to_tibble` is no longer needed and can be dropped.

`convert_nipa_row_to_tibble` is no longer called and can be removed. `extract_regional_line_info` (line 182) is also no longer called after the 2b fix below and can be removed. `process_bea_series_groups` (line 383) is defined but never called anywhere in the codebase and should also be removed as dead code.

### 2b. Fix for `fetch_bea_regional` (lines 552-579)

The same `purrr::map_dfr` anti-pattern applies here, with the additional complication that `extract_regional_line_info` is called per-row.

Replace the entire `purrr::map_dfr` block:

```r
purrr::map_dfr(raw_data, function(row) {
  line_info = extract_regional_line_info(
    row$Code,
    row$Description,
    line_code,
    statistic
  )
  time_period = row$TimePeriod %||% NA_character_
  tibble::tibble(
    geo_fips = row$GeoFips %||% NA_character_,
    ...
  )
})
```

With vectorized construction:

```r
# Vectorize line info extraction
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

The key change for `extract_regional_line_info` is replacing the per-row `strsplit(code, "-")[[1]]` with vectorized `sub(".*-", "", raw_data$Code)`, which extracts everything after the last hyphen using a regex. The scalar values (`table_name`, `public_table`, `statistic`, `line_code`) are recycled by `tibble()`.

`normalize_api_frequency`, `parse_api_date`, and `extract_period_component` must be vectorized (Bottlenecks 3 and 4) before this works on vectors. They are already called on vectors in `process_nipa_dates_and_values`, so after Bottlenecks 3 and 4 are fixed, this is safe.

Note: until this Bottleneck 2b fix is applied, the date helper vectorizations (Bottlenecks 3 and 4) provide minimal benefit to the regional path, because `fetch_bea_regional` currently calls them with scalar values inside a per-row `map_dfr` loop. The full speedup for the regional path requires all four bottlenecks.

## Bottleneck 3: Element-by-element date parsing (High impact)

**Location:** `parse_bea_date` in `R/date_helpers.R`, lines 97-116.

**Problem:** Each date string is processed individually with `purrr::map`. R's `grepl`, `substr`, and `lubridate::yq`/`lubridate::ym` are all vectorized, so there is no need to loop.

**Fix:** Use a subset-and-reassemble approach. Pre-allocate the result vector, classify each element by format, then parse each subset with the appropriate vectorized function:

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

**Why not `dplyr::case_when`?** `case_when` evaluates all right-hand side expressions for the full input vector before selecting which result to use per element. On mixed-format input (e.g., `c("2024Q1", "2024")`), every RHS is evaluated against every element. This causes two problems: `lubridate::yq("2024")` produces `NA` with a **warning**, and `as.Date(paste0("2024Q1", "-01-01"))` throws a **hard error** (`"character string is not in a standard unambiguous format"`). The annual branch's `as.Date()` is the one that makes `case_when` genuinely impossible here (not just noisy). The subset-and-reassemble approach avoids this by only passing matching elements to each parser.

Note: `normalize_api_frequency` already uses `case_when` successfully because all its RHS values are simple string constants (`"quarter"`, `"month"`, `"year"`), not function calls that can fail on non-matching inputs.

## Bottleneck 4: Element-by-element period extraction (High impact)

**Location:** `extract_period_component` in `R/date_helpers.R`, lines 128-164.

**Problem:** Each of the three extractions (year, quarter, month) uses `purrr::map_int` to iterate element-by-element. This function is called three times per table (once each for year, quarter, and month), so every `time_period` string gets looped over three separate times.

**Fix:** Use the same subset-and-reassemble pattern as Bottleneck 3. Pre-allocate an `NA_integer_` vector, then only call `as.integer(substr(...))` on the subset of elements that match the format. This avoids both the loop overhead and the spurious "NAs introduced by coercion" warnings that a naive `ifelse` replacement would produce:

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
      # The regex guarantees format "YYYYQn" (6 chars), so position 6 is always
      # the quarter digit. Using a literal 6 instead of nchar() is clearer.
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

## Implementation order

The fixes should be applied in this order:

1. **Bottlenecks 3 and 4** (date helpers): These are self-contained changes in `R/date_helpers.R` with no dependencies on other bottlenecks. They can be verified independently by running the existing tests. Implement and test these first.

2. **Bottlenecks 1 and 2 together** (JSON parsing + vectorized tibble construction): These must be applied as a **single atomic commit**. Change `simplifyVector` to `TRUE`, adapt all affected helper functions (`extract_all_note_texts`, `extract_table_description`), and simultaneously refactor `fetch_bea_nipa_complete` and `fetch_bea_regional` to construct tibbles from column vectors. Remove dead code: `convert_nipa_row_to_tibble`, `extract_regional_line_info`, and `process_bea_series_groups`.

After each step, run `devtools::test()` to verify no regressions.

### Unit tests for date helpers

There are currently no unit tests for `parse_bea_date` or `extract_period_component`. Before rewriting these functions, add tests in `tests/testthat/` covering:
- Mixed-frequency inputs (e.g., `c("2024Q1", "2024", "2024M06")`)
- All-`NA` inputs
- Empty vector inputs
- Unrecognized format inputs (e.g., `"garbage"`)

These serve as a regression safety net when swapping implementations. `grepl(pattern, NA)` returns `FALSE` in R, so `NA` elements correctly pass through as `NA` in the output — the tests should verify this.

## Summary

| # | Bottleneck | Location | Impact | Fix |
|---|---|---|---|---|
| 1 | `simplifyVector = FALSE` | `R/bea.R` L88 | High | Change to `TRUE`; adapt `extract_all_note_texts`, `extract_table_description` |
| 2 | Row-by-row tibble construction | `R/bea.R` L343-346, L553-579 | High | Vectorized tibble from columns in both `fetch_bea_nipa_complete` and `fetch_bea_regional` |
| 3 | Element-by-element date parsing | `R/date_helpers.R` L97-116 | High | Subset-and-reassemble with vectorized `lubridate` calls |
| 4 | Element-by-element period extraction | `R/date_helpers.R` L128-164 | High | Subset-and-reassemble with vectorized `substr` + `as.integer` |

Bottlenecks 1 and 2 must be applied as a single atomic commit and together should yield the largest speedup (likely an order of magnitude for large tables). Bottlenecks 3 and 4 provide substantial additional improvement and are safe to implement first as independent changes.

### Dead code to remove

After all fixes are applied, the following functions are no longer called and should be removed:
- `convert_nipa_row_to_tibble` (line 142) — replaced by vectorized tibble construction in 2a
- `extract_regional_line_info` (line 182) — replaced by inline vectorized code in 2b
- `process_bea_series_groups` (line 383) — never called anywhere in the codebase
