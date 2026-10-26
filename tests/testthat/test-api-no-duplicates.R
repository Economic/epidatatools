# Tests to ensure API functions do not return duplicate data
# These call the live APIs: see helper-live-api.R for when they run and skip

# Helper function to check for duplicate rows
has_no_duplicates <- function(data, key_cols) {
  n_rows <- nrow(data)
  n_distinct <- data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols))) |>
    nrow()
  n_rows == n_distinct
}

# get_bls() tests ----

# Two calls (4 BLS requests) cover: several series, named and unnamed, monthly,
# quarterly and semiannual, and a QCEW series that has no catalog data
bls_series_ids <- c(
  emp_fb_2534 = "LNU02073399",
  epop_asianmen_2554 = "LNU02332330Q",
  cpi_semi = "CUUS0000SA0",
  "LNU02300060",
  qcew = "ENUUS00020510"
)

test_that("get_bls() returns no duplicate rows, including a series with no catalog", {
  skip_unless_live_api("BLS_API_KEY")

  result <- live_api(get_bls(bls_series_ids, start = 2024, end = 2024))

  expect_true(
    has_no_duplicates(result, c("series_id", "date")),
    info = "Each series should have no duplicate series_id + date combinations"
  )
  expect_setequal(unique(result$series_id), unname(bls_series_ids))

  # QCEW series do not have catalog data, but their data is returned
  qcew <- result[result$series_id == "ENUUS00020510", ]
  expect_true(nrow(qcew) > 0, info = "Should return data even without catalog")
  expect_true(all(is.na(qcew$series_title)))
})

test_that("get_bls() with metadata returns one row per series", {
  skip_unless_live_api("BLS_API_KEY")

  result <- live_api(get_bls(bls_series_ids, start = 2024, end = 2024, metadata = TRUE))

  expect_true(
    has_no_duplicates(result, c("series_id")),
    info = "Metadata results should have one row per series_id"
  )
  expect_equal(nrow(result), length(bls_series_ids))
})

# get_bea_nipa() tests ----

test_that("get_bea_nipa() returns no duplicate rows for single table", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_nipa("T10101", years = 2023:2024, frequency = "year"))

  expect_true(
    has_no_duplicates(result, c("table_name", "line_number", "date")),
    info = "Single table should have no duplicate table + line + date combinations"
  )
})

test_that("get_bea_nipa() returns no duplicate rows for multiple tables", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_nipa(
    c("gdp" = "T10101", "income" = "T20100"),
    years = 2023:2024,
    frequency = "year"
  ))

  expect_true(
    has_no_duplicates(result, c("table_name", "line_number", "date")),
    info = "Multiple tables should have no duplicate table + line + date combinations"
  )

  # Verify we have the expected number of unique tables
  n_tables <- length(unique(result$table_name))
  expect_equal(n_tables, 2)
})

test_that("get_bea_nipa() assigns correct names when same table appears multiple times", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_nipa(
    c("comp" = "T11400", "surp" = "T11400"),
    years = 2001,
    frequency = "quarter"
  )) |>
    dplyr::filter(line_number == 1)

  # Both names should appear
  expect_true("comp" %in% result$name, info = "Should have 'comp' name")
  expect_true("surp" %in% result$name, info = "Should have 'surp' name")

  # Each name should have the same number of rows
  name_counts <- table(result$name)
  expect_equal(unname(name_counts[["comp"]]), unname(name_counts[["surp"]]))
})

test_that("get_bea_nipa() returns no duplicate rows for quarterly data", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_nipa("T10101", years = 2024, frequency = "quarter"))

  expect_true(
    has_no_duplicates(result, c("table_name", "line_number", "date")),
    info = "Quarterly data should have no duplicate combinations"
  )
})

# get_bea_regional() tests ----

test_that("get_bea_regional() returns no duplicate rows for single geo", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_regional(
    geo_fips = "36000",
    table_name = "SAINC1",
    line_code = 1,
    year = 2020:2023
  ))

  expect_true(
    has_no_duplicates(result, c("geo_fips", "table_name", "line_number", "date")),
    info = "Single geo should have no duplicate geo + table + line + date combinations"
  )
})

test_that("get_bea_regional() returns no duplicate rows for multiple geos", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_regional(
    geo_fips = c("36000", "06000"),
    table_name = "SAINC1",
    line_code = 1,
    year = 2020:2023
  ))

  expect_true(
    has_no_duplicates(result, c("geo_fips", "table_name", "line_number", "date")),
    info = "Multiple geos should have no duplicate combinations"
  )

  # Verify we have the expected number of unique geos
  n_geos <- length(unique(result$geo_fips))
  expect_equal(n_geos, 2)
})

test_that("get_bea_regional() returns no duplicate rows with metadata", {
  skip_unless_live_api("BEA_API_KEY")

  result <- live_api(get_bea_regional(
    geo_fips = "36000",
    table_name = "SAINC1",
    line_code = 1,
    year = 2022:2023,
    metadata = TRUE
  ))

  expect_true(
    has_no_duplicates(result, c("geo_fips", "table_name", "line_number", "date")),
    info = "Metadata results should have no duplicate combinations"
  )
})
