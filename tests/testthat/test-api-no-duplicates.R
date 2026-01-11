# Tests to ensure API functions do not return duplicate data
# These tests require API keys to be set in the environment

# Helper function to check for duplicate rows
has_no_duplicates <- function(data, key_cols) {
  n_rows <- nrow(data)
  n_distinct <- data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols))) |>
    nrow()
  n_rows == n_distinct
}

# get_bls() tests ----

test_that("get_bls() returns no duplicate rows for single series", {
  skip_if_not(
    nzchar(Sys.getenv("BLS_API_KEY")),
    "BLS_API_KEY not set"
  )

  result <- get_bls("LNU02300060", start = 2024, end = 2024)

  expect_true(
    has_no_duplicates(result, c("series_id", "date")),
    info = "Single series should have no duplicate series_id + date combinations"
  )
})

test_that("get_bls() returns no duplicate rows for multiple series", {
  skip_if_not(
    nzchar(Sys.getenv("BLS_API_KEY")),
    "BLS_API_KEY not set"
  )

  bls_series_ids <- c(
    emp_fb_2534 = "LNU02073399",
    epop_asianmen_2554 = "LNU02332330Q",
    cpi_semi = "CUUS0000SA0",
    "LNU02300060"
  )

  result <- get_bls(bls_series_ids, start = 2024, end = 2024)

  expect_true(
    has_no_duplicates(result, c("series_id", "date")),
    info = "Multiple series should have no duplicate series_id + date combinations"
  )

  # Verify we have the expected number of unique series
  n_series <- length(unique(result$series_id))
  expect_equal(n_series, length(bls_series_ids))
})

test_that("get_bls() with metadata returns no duplicate rows", {
  skip_if_not(
    nzchar(Sys.getenv("BLS_API_KEY")),
    "BLS_API_KEY not set"
  )

  bls_series_ids <- c(
    emp_fb_2534 = "LNU02073399",
    "LNU02300060"
  )

  result <- get_bls(bls_series_ids, start = 2024, end = 2024, metadata = TRUE)

  expect_true(
    has_no_duplicates(result, c("series_id")),
    info = "Metadata results should have one row per series_id"
  )
})

# get_bea_nipa() tests ----

test_that("get_bea_nipa() returns no duplicate rows for single table", {

  skip_if_not(
    nzchar(Sys.getenv("BEA_API_KEY")),
    "BEA_API_KEY not set"
  )

  result <- get_bea_nipa("T10101", years = 2023:2024, frequency = "year")

  expect_true(
    has_no_duplicates(result, c("table_name", "line_number", "date")),
    info = "Single table should have no duplicate table + line + date combinations"
  )
})

test_that("get_bea_nipa() returns no duplicate rows for multiple tables", {
  skip_if_not(
    nzchar(Sys.getenv("BEA_API_KEY")),
    "BEA_API_KEY not set"
  )

  result <- get_bea_nipa(
    c("gdp" = "T10101", "income" = "T20100"),
    years = 2023:2024,
    frequency = "year"
  )

  expect_true(
    has_no_duplicates(result, c("table_name", "line_number", "date")),
    info = "Multiple tables should have no duplicate table + line + date combinations"
  )

  # Verify we have the expected number of unique tables
  n_tables <- length(unique(result$table_name))
  expect_equal(n_tables, 2)
})

test_that("get_bea_nipa() returns no duplicate rows for quarterly data", {
  skip_if_not(
    nzchar(Sys.getenv("BEA_API_KEY")),
    "BEA_API_KEY not set"
  )

  result <- get_bea_nipa("T10101", years = 2024, frequency = "quarter")

  expect_true(
    has_no_duplicates(result, c("table_name", "line_number", "date")),
    info = "Quarterly data should have no duplicate combinations"
  )
})

# get_bea_regional() tests ----

test_that("get_bea_regional() returns no duplicate rows for single geo", {
  skip_if_not(
    nzchar(Sys.getenv("BEA_API_KEY")),
    "BEA_API_KEY not set"
  )

  result <- get_bea_regional(
    geo_fips = "36000",
    table_name = "SAINC1",
    line_code = 1,
    year = 2020:2023
  )

  expect_true(
    has_no_duplicates(result, c("geo_fips", "table_name", "line_number", "date")),
    info = "Single geo should have no duplicate geo + table + line + date combinations"
  )
})

test_that("get_bea_regional() returns no duplicate rows for multiple geos", {
  skip_if_not(
    nzchar(Sys.getenv("BEA_API_KEY")),
    "BEA_API_KEY not set"
  )

  result <- get_bea_regional(
    geo_fips = c("36000", "06000"),
    table_name = "SAINC1",
    line_code = 1,
    year = 2020:2023
  )

  expect_true(
    has_no_duplicates(result, c("geo_fips", "table_name", "line_number", "date")),
    info = "Multiple geos should have no duplicate combinations"
  )

  # Verify we have the expected number of unique geos
  n_geos <- length(unique(result$geo_fips))
  expect_equal(n_geos, 2)
})

test_that("get_bea_regional() returns no duplicate rows with metadata", {
  skip_if_not(
    nzchar(Sys.getenv("BEA_API_KEY")),
    "BEA_API_KEY not set"
  )

  result <- get_bea_regional(
    geo_fips = "36000",
    table_name = "SAINC1",
    line_code = 1,
    year = 2022:2023,
    metadata = TRUE
  )

  expect_true(
    has_no_duplicates(result, c("geo_fips", "table_name", "line_number", "date")),
    info = "Metadata results should have no duplicate combinations"
  )
})
