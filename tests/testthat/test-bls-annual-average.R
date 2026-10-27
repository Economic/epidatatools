# Offline API fixtures include native periods and official annual averages.
# The values differ deliberately: get_bls() must not calculate its own average.
annual_obs = function(year, period, value, note = NULL) {
  list(
    year = as.character(year), period = period, periodName = "Annual",
    value = as.character(value),
    footnotes = if (is.null(note)) list(list()) else list(list(text = note))
  )
}

annual_api = function(calls, unavailable = character()) {
  function(series_ids, start, end, catalog = FALSE, bls_api_key = "",
           annual_average = FALSE) {
    calls$requests = c(calls$requests, list(list(
      ids = series_ids, start = start, end = end, catalog = catalog,
      annual_average = annual_average
    )))
    lapply(series_ids, function(id) {
      periods = switch(id, SEMI = c("S01", "S02", "S03"),
                       QUARTER = c("Q01", "Q04", "Q05"),
                       ANNUAL = c("A01"), c("M01", "M12", "M13"))
      if (!annual_average || id %in% unavailable) {
        periods = periods[!is_bls_annual_average_period(periods)]
      }
      observations = unlist(lapply(start:end, function(y) {
        lapply(seq_along(periods), function(i) {
          annual_obs(y, periods[i], i * 10, "Published footnote.")
        })
      }), recursive = FALSE)
      s = list(seriesID = id, data = observations)
      if (catalog) s$catalog = list(series_title = paste("Title", id), series_id = id)
      s
    })
  }
}

annual_calls = function() {
  calls = new.env()
  calls$requests = list()
  calls
}

test_that("annual averages are classified and dated as years, including Q05", {
  periods = c("M01", "M13", "S01", "S02", "S03", "Q04", "Q05", "A01")
  frequency = normalize_api_frequency(periods, "bls")
  expect_equal(frequency, c("month", "year", "semiyear", "semiyear", "year",
                            "quarter", "year", "year"))
  expect_no_warning(dates <- parse_bls_date(frequency, rep(2025, 8), periods))
  expect_equal(dates, as.Date(c("2025-01-01", "2025-01-01", "2025-01-01",
                                "2025-07-01", "2025-01-01", "2025-10-01",
                                "2025-01-01", "2025-01-01")))
})

test_that("annual_average returns only published annual values in a flat tibble", {
  calls = annual_calls()
  local_mocked_bindings(get_bls_api = annual_api(calls))
  result = get_bls(c(monthly = "MONTH", semi = "SEMI", quarterly = "QUARTER"),
                   2024, 2025, bls_api_key = "fake", annual_average = TRUE)
  expect_equal(nrow(result), 6L)
  expect_equal(result$value, rep(30, 6))
  expect_equal(result$name, rep(c("monthly", "semi", "quarterly"), each = 2))
  expect_equal(unique(result$date_frequency), "year")
  expect_equal(result$date, rep(as.Date(c("2024-01-01", "2025-01-01")), 3))
  expect_equal(result$year, rep(2024:2025, 3))
  expect_false(any(c("month", "quarter", "semiyear") %in% names(result)))
})

test_that("metadata output keeps annual periods, numeric values, and footnotes", {
  calls = annual_calls()
  local_mocked_bindings(get_bls_api = annual_api(calls))
  result = get_bls("SEMI", 2024, 2025, metadata = TRUE,
                   bls_api_key = "fake", annual_average = TRUE)
  data = result$data[[1]]
  expect_equal(data$period, rep("S03", 2))
  expect_equal(data$date_frequency, rep("year", 2))
  expect_equal(data$date, as.Date(c("2024-01-01", "2025-01-01")))
  expect_type(data$value, "double")
  expect_equal(data$value, c(30, 30))
  expect_equal(data$footnotes, rep("Published footnote.", 2))
})

test_that("default and positional calls keep native observations", {
  calls = annual_calls()
  local_mocked_bindings(get_bls_api = annual_api(calls))
  # Preserve the existing fifth positional argument (API key).
  usual = get_bls("MONTH", 2025, 2025, FALSE, "fake")
  explicit = get_bls("MONTH", 2025, 2025, FALSE, "fake", annual_average = FALSE)
  expect_identical(usual, explicit)
  expect_equal(usual$value, c(10, 20))
  expect_equal(usual$month, c(1, 12))
  expect_equal(unique(usual$date_frequency), "month")
  expect_false(any(vapply(calls$requests, function(x) x$annual_average, logical(1))))
})

test_that("annual requests span every series batch and year range", {
  calls = annual_calls()
  local_mocked_bindings(get_bls_api = annual_api(calls))
  ids = sprintf("S%03d", 1:51)
  result = get_bls(ids, 2000, 2020, bls_api_key = "fake", annual_average = TRUE)
  requests = Filter(function(x) !x$catalog, calls$requests)
  expect_length(requests, 4)
  expect_true(all(vapply(requests, function(x) x$annual_average, logical(1))))
  expect_equal(nrow(result), 51 * 21)
  expect_equal(nrow(dplyr::distinct(result, series_id, year)), nrow(result))
  expect_equal(range(result$year), c(2000, 2020))
  expect_equal(unique(result$date_frequency), "year")
})

test_that("annual_average must be one nonmissing logical before any API request", {
  calls = annual_calls()
  local_mocked_bindings(get_bls_api = annual_api(calls))
  for (bad in list(NA, NULL, logical(), c(TRUE, FALSE), "TRUE", 1)) {
    expect_error(get_bls("MONTH", 2025, 2025, bls_api_key = "fake",
                         annual_average = bad), "`annual_average` must be TRUE or FALSE", fixed = TRUE)
  }
  expect_length(calls$requests, 0)
})

test_that("missing averages warn without manufacturing values or dropping metadata", {
  calls = annual_calls()
  local_mocked_bindings(get_bls_api = annual_api(calls, unavailable = "MONTH"))
  w = expect_warning(
    result <- get_bls(c("MONTH", "SEMI"), 2025, 2025,
                      bls_api_key = "fake", annual_average = TRUE),
    class = "bls_annual_average_unavailable"
  )
  expect_equal(w$series_ids, "MONTH")
  expect_equal(result$series_id, "SEMI")
  expect_equal(result$value, 30)
  expect_warning(
    nested <- get_bls("MONTH", 2025, 2025, metadata = TRUE,
                      bls_api_key = "fake", annual_average = TRUE),
    class = "bls_annual_average_unavailable"
  )
  expect_equal(nested$series_id, "MONTH")
  expect_equal(nrow(nested$data[[1]]), 0)
  expect_type(nested$data[[1]]$value, "double")
  expect_warning(
    empty <- get_bls("MONTH", 2025, 2025, bls_api_key = "fake", annual_average = TRUE),
    class = "bls_annual_average_unavailable"
  )
  expect_equal(nrow(empty), 0)
  expect_s3_class(empty$date, "Date")
  expect_type(empty$value, "double")
  # A native annual observation is not automatically an annual average.
  expect_warning(
    annual <- get_bls("ANNUAL", 2025, 2025, bls_api_key = "fake", annual_average = TRUE),
    class = "bls_annual_average_unavailable"
  )
  expect_equal(nrow(annual), 0)
})

test_that("annual-average requests send BLS's annualaverage payload option", {
  captured = new.env()
  local_mocked_bindings(POST = function(url, body, encode) {
    captured$body = body
    structure(list(
      status_code = 200L, url = url, headers = list(`Content-Type` = "application/json"),
      content = charToRaw('{"status":"REQUEST_SUCCEEDED","Results":{"series":[]}}')
    ), class = "response")
  }, .package = "httr")
  get_bls_api("SEMI", 2025, 2025, bls_api_key = "fake", annual_average = TRUE)
  expect_true(captured$body$annualaverage)
  expect_equal(captured$body$seriesid, list("SEMI"))
  get_bls_api("SEMI", 2025, 2025, bls_api_key = "fake")
  expect_null(captured$body$annualaverage)
})
