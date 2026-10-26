# BLS sends a value that is not a number, such as "-", when it has no data, and
# explains why in a footnote. These tests mock get_bls_api() with the shapes BLS
# returned for the 2025 lapse in appropriations, so no test calls the API.

shutdown_note = "Data unavailable due to the 2025 lapse in appropriations."
quarter_note = paste(
  "Data for Q4 2025 were not produced.",
  "Data for October 2025 were not collected due to the federal government shutdown."
)

bls_obs = function(year, period, period_name, value, notes = character()) {
  list(
    year = year,
    period = period,
    periodName = period_name,
    value = value,
    footnotes = if (length(notes) == 0) {
      list(list())
    } else {
      purrr::map(notes, function(n) list(code = "9", text = n))
    }
  )
}

# A fake get_bls_api() serving `series`, a list of observation lists named by ID
fake_bls_series_api = function(series) {
  function(series_ids, start, end, catalog = FALSE, bls_api_key = "") {
    purrr::map(series_ids, function(id) {
      s = list(seriesID = id, data = series[[id]])
      if (catalog) {
        s$catalog = list(series_title = paste("Title", id), series_id = id)
      }
      s
    })
  }
}

gap_series = list(
  MONTHLY = list(
    bls_obs("2025", "M11", "November", "4.5"),
    bls_obs("2025", "M10", "October", "-", shutdown_note),
    bls_obs("2025", "M09", "September", "4.1", c("Note one.", "Note two."))
  ),
  QUARTERLY = list(
    bls_obs("2025", "Q04", "4th Quarter", "-", quarter_note),
    bls_obs("2025", "Q03", "3rd Quarter", "61.2")
  )
)

test_that("missing values are NA, with BLS's explanation instead of a coercion warning", {
  local_mocked_bindings(get_bls_api = fake_bls_series_api(gap_series))

  w = expect_warning(
    result <- get_bls(c("MONTHLY", "QUARTERLY"), 2025, 2025, bls_api_key = "fake"),
    class = "bls_missing_values"
  )
  msg = conditionMessage(w)
  expect_match(msg, "no value for 2 observations", fixed = TRUE)
  expect_match(msg, "lapse in appropriations (October 2025 for MONTHLY)", fixed = TRUE)
  expect_match(msg, "4th Quarter 2025 for QUARTERLY", fixed = TRUE)
  expect_match(msg, "epidatatools.warn_missing", fixed = TRUE)
  expect_false(grepl("coercion", msg))
  expect_equal(w$observations$series_id, c("MONTHLY", "QUARTERLY"))

  expect_type(result$value, "double")
  expect_equal(result$value, c(4.1, NA, 4.5, 61.2, NA))
  expect_named(
    result,
    c("series_id", "series_title", "date_frequency", "date", "year", "quarter", "month", "value")
  )
})

test_that("many series with one footnote are listed briefly", {
  ids = sprintf("S%02d", 1:8)
  series = purrr::set_names(
    purrr::map(ids, function(id) list(bls_obs("2025", "M10", "October", "-", shutdown_note))),
    ids
  )
  local_mocked_bindings(get_bls_api = fake_bls_series_api(series))
  w = expect_warning(get_bls(ids, 2025, 2025, bls_api_key = "fake"), class = "bls_missing_values")
  expect_match(conditionMessage(w), "October 2025 for S01, S02, S03, S04, S05 and 3 more", fixed = TRUE)
})

test_that("options(epidatatools.warn_missing = FALSE) turns off the warning", {
  local_mocked_bindings(get_bls_api = fake_bls_series_api(gap_series))
  withr::local_options(epidatatools.warn_missing = FALSE)
  expect_no_warning(result <- get_bls("MONTHLY", 2025, 2025, bls_api_key = "fake"))
  expect_true(is.na(result$value[result$month == 10]))
})

test_that("values that are not numbers and have no footnote get their own warning", {
  odd = list(ODD = list(bls_obs("2025", "M01", "January", "N.A."), bls_obs("2025", "M02", "February", "3")))
  local_mocked_bindings(get_bls_api = fake_bls_series_api(odd))
  # The option for known gaps does not hide these
  withr::local_options(epidatatools.warn_missing = FALSE)

  w = expect_warning(
    result <- get_bls("ODD", 2025, 2025, bls_api_key = "fake"),
    class = "bls_unexpected_values"
  )
  expect_match(conditionMessage(w), 'ODD, January 2025: "N.A."', fixed = TRUE)
  expect_equal(result$value, c(NA, 3))
})

test_that("metadata = TRUE data has numeric values and one footnotes column", {
  local_mocked_bindings(get_bls_api = fake_bls_series_api(gap_series))
  result = suppressWarnings(
    get_bls("MONTHLY", 2025, 2025, metadata = TRUE, bls_api_key = "fake"),
    classes = "bls_missing_values"
  )
  data = result$data[[1]]
  expect_type(data$value, "double")
  # One row per observation, even with two footnotes
  expect_equal(nrow(data), 3)
  expect_equal(data$footnotes, c("Note one.; Note two.", shutdown_note, NA))
})

test_that("no warning when every value is a number", {
  plain = list(PLAIN = list(bls_obs("2025", "M01", "January", "1.5", "Preliminary.")))
  local_mocked_bindings(get_bls_api = fake_bls_series_api(plain))
  expect_no_warning(result <- get_bls("PLAIN", 2025, 2025, bls_api_key = "fake"))
  expect_equal(result$value, 1.5)
})
