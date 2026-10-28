# Fixed-year responses reproduce #46 without spending API requests. BLS returns
# native observations for 2026, but annual averages only through 2025.
coverage_obs = function(year, period = "M13", value = "4", note = NULL) {
  list(
    year = as.character(year), period = period, periodName = "Annual",
    value = value,
    footnotes = if (is.null(note)) list(list()) else list(list(text = note))
  )
}

coverage_api = function(calls, observations, notes = character()) {
  function(series_ids, start, end, catalog = FALSE, bls_api_key = "",
           annual_average = FALSE) {
    calls$requests = c(calls$requests, list(list(
      ids = series_ids, start = start, end = end, catalog = catalog
    )))
    if (start > 2026) {
      rlang::abort("startyear is invalid.", class = "bls_api_error",
                   status = "REQUEST_FAILED_INVALID_PARAMETERS")
    }
    series = lapply(series_ids, function(id) {
      data = Filter(function(x) {
        as.integer(x$year) >= start && as.integer(x$year) <= end &&
          (annual_average || !is_bls_annual_average_period(x$period))
      }, observations[[id]])
      result = list(seriesID = id, data = data)
      if (catalog) result$catalog = list(series_title = paste("Title", id))
      result
    })
    process_bls_response(list(
      status = "REQUEST_SUCCEEDED", message = as.list(notes),
      Results = list(series = series)
    ), bls_api_key)
  }
}

coverage_calls = function() {
  calls = new.env()
  calls$requests = list()
  calls
}

coverage_recent = list(
  LNU04000000 = list(coverage_obs(2024), coverage_obs(2025),
                     coverage_obs(2024, "M01"), coverage_obs(2025, "M01"),
                     coverage_obs(2026, "M01"))
)

coverage_capture = function(expr) {
  warnings = messages = list()
  result = withCallingHandlers(
    expr,
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages[[length(messages) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  list(result = result, warnings = warnings, messages = messages)
}

test_that("partial annual coverage identifies each missing requested year", {
  calls = coverage_calls()
  local_mocked_bindings(get_bls_api = coverage_api(calls, coverage_recent),
                        bls_current_year = function() 2026L)
  w = expect_warning(
    result <- get_bls("LNU04000000", 2024, 2026, annual_average = TRUE,
                      bls_api_key = "fake"),
    class = "bls_annual_average_unavailable"
  )
  expect_equal(result$year, 2024:2025)
  expect_equal(w$missing_years, tibble::tibble(series_id = "LNU04000000", year = 2026L))
  expect_length(calls$requests, 2)
})

test_that("future end years preserve data and use a valid catalog year", {
  calls = coverage_calls()
  local_mocked_bindings(get_bls_api = coverage_api(calls, coverage_recent),
                        bls_current_year = function() 2026L)
  w = expect_warning(
    result <- get_bls("LNU04000000", 2024, 2027, annual_average = TRUE,
                      bls_api_key = "fake"),
    class = "bls_annual_average_unavailable"
  )
  expect_equal(result$year, 2024:2025)
  expect_equal(w$missing_years$year, 2026:2027)
  expect_length(calls$requests, 2)
  expect_equal(calls$requests[[2]]$start, 2024)
  expect_equal(calls$requests[[2]]$end, 2024)
})

test_that("native frequency reports absent whole years for flat and nested output", {
  calls = coverage_calls()
  local_mocked_bindings(
    get_bls_api = coverage_api(calls, coverage_recent,
                               "No Data Available for Series LNU04000000 Year: 2027"),
    bls_current_year = function() 2026L
  )
  for (nested in c(FALSE, TRUE)) {
    captured = coverage_capture(get_bls("LNU04000000", 2024, 2027,
                                        metadata = nested, bls_api_key = "fake"))
    expect_length(captured$warnings, 1)
    expect_length(captured$messages, 0)
    w = captured$warnings[[1]]
    expect_s3_class(w, "bls_years_unavailable")
    expect_false(inherits(w, "bls_annual_average_unavailable"))
    expect_equal(w$missing_years, tibble::tibble(series_id = "LNU04000000", year = 2027L))
    data = if (nested) captured$result$data[[1]] else captured$result
    expect_equal(nrow(data), 3)
    expect_equal(as.integer(data$year), 2024:2026)
  }
  expect_length(calls$requests, 4)
})

test_that("historical gaps replace no-data messages with one annual warning", {
  calls = coverage_calls()
  history = list(LNU04000000 = lapply(1947:1949, coverage_obs))
  local_mocked_bindings(
    get_bls_api = coverage_api(calls, history, c(
      "No Data Available for Series LNU04000000 Year: 1945",
      "No Data Available for Series LNU04000000 Year: 1946",
      "An unrelated BLS note."
    )),
    bls_current_year = function() 2026L
  )
  captured = coverage_capture(get_bls("LNU04000000", 1945, 1949,
                                      annual_average = TRUE, bls_api_key = "fake"))
  expect_length(captured$warnings, 1)
  expect_equal(captured$warnings[[1]]$missing_years$year, 1945:1946)
  expect_match(conditionMessage(captured$warnings[[1]]), "1945\u20131946", fixed = TRUE)
  expect_equal(captured$result$year, 1947:1949)
  expect_length(captured$messages, 1)
  expect_match(conditionMessage(captured$messages[[1]]), "An unrelated BLS note.", fixed = TRUE)
  expect_length(calls$requests, 2)
  expect_equal(calls$requests[[2]]$start, 1945)
})

test_that("mixed partial and empty annual series share identical flat/nested diagnostics", {
  calls = coverage_calls()
  observations = c(coverage_recent, list(EMPTY = list(coverage_obs(2026, "M01"))))
  local_mocked_bindings(get_bls_api = coverage_api(calls, observations),
                        bls_current_year = function() 2026L)
  captured = lapply(c(FALSE, TRUE), function(nested) {
    coverage_capture(get_bls(c(named = "LNU04000000", "EMPTY"), 2024, 2027,
                             annual_average = TRUE, metadata = nested, bls_api_key = "fake"))
  })
  expect_equal(vapply(captured, function(x) length(x$warnings), integer(1)), c(1L, 1L))
  flat = captured[[1]]$warnings[[1]]
  nested = captured[[2]]$warnings[[1]]
  expect_s3_class(flat, "bls_annual_average_unavailable")
  expect_s3_class(flat, "bls_years_unavailable")
  expect_equal(flat$series_ids, c("LNU04000000", "EMPTY"))
  expect_equal(flat$missing_years, tibble::tibble(
    series_id = c(rep("LNU04000000", 2), rep("EMPTY", 4)),
    year = c(2026:2027, 2024:2027)
  ))
  expect_identical(flat$missing_years, nested$missing_years)
  expect_identical(conditionMessage(flat), conditionMessage(nested))
  expect_equal(nrow(captured[[1]]$result), 2)
  expect_equal(vapply(captured[[2]]$result$data, nrow, integer(1)), c(2L, 0L))
  expect_length(calls$requests, 4)
})

test_that("future-only batches are avoided while all requested gaps are reported", {
  calls = coverage_calls()
  ids = sprintf("S%03d", 1:51)
  observations = stats::setNames(lapply(ids, function(id) {
    lapply(2007:2026, coverage_obs)
  }), ids)
  local_mocked_bindings(get_bls_api = coverage_api(calls, observations),
                        bls_current_year = function() 2026L)
  captured = coverage_capture(get_bls(ids, 1987, 2047, annual_average = TRUE,
                                      bls_api_key = "fake"))
  # 2 series batches x 2 bounded year spans, plus 2 catalog requests.
  expect_length(calls$requests, 6)
  expect_true(all(vapply(calls$requests, function(x) x$start <= 2026 && x$end <= 2026,
                         logical(1))))
  expect_length(captured$warnings, 1)
  w = captured$warnings[[1]]
  expect_equal(w$series_ids, ids)
  expect_equal(w$missing_years$year[w$missing_years$series_id == ids[1]],
                c(1987:2006, 2027:2047))
  expect_equal(nrow(w$missing_years), 51 * 41)
  expect_equal(nrow(captured$result), 51 * 20)
  expect_match(conditionMessage(w), "46 more series not shown", fixed = TRUE)
})

test_that("entirely future intervals fail before any API requests", {
  calls = coverage_calls()
  local_mocked_bindings(get_bls_api = coverage_api(calls, coverage_recent),
                        bls_current_year = function() 2026L)
  for (annual in c(FALSE, TRUE)) {
    e = expect_error(get_bls("LNU04000000", 2027, 2047, annual_average = annual,
                             bls_api_key = "fake"), class = "bls_future_interval")
    expect_match(conditionMessage(e), "current year (2026)", fixed = TRUE)
    expect_equal(e$start, 2027)
    expect_equal(e$end, 2047)
    expect_equal(e$current_year, 2026L)
  }
  expect_length(calls$requests, 0)
})

test_that("NA observations remain present and missing-value warnings stay selective", {
  calls = coverage_calls()
  observations = list(NA_SERIES = list(coverage_obs(2025, value = "-", note = "BLS footnote.")))
  local_mocked_bindings(get_bls_api = coverage_api(calls, observations),
                        bls_current_year = function() 2026L)
  for (annual in c(FALSE, TRUE)) {
    # Use a native annual observation for the default-frequency case.
    observations$NA_SERIES[[1]]$period = if (annual) "M13" else "A01"
    local_mocked_bindings(get_bls_api = coverage_api(calls, observations))
    captured = coverage_capture(suppressWarnings(
      get_bls("NA_SERIES", 2025, 2026, annual_average = annual,
              metadata = TRUE, bls_api_key = "fake"), classes = "bls_years_unavailable"
    ))
    expect_length(captured$warnings, 1)
    expect_s3_class(captured$warnings[[1]], "bls_missing_values")
    expect_true(is.na(captured$result$data[[1]]$value))
    expect_equal(captured$result$data[[1]]$footnotes, "BLS footnote.")
    captured = coverage_capture(suppressWarnings(
      get_bls("NA_SERIES", 2025, 2026, annual_average = annual,
              bls_api_key = "fake"), classes = "bls_missing_values"
    ))
    expect_length(captured$warnings, 1)
    expect_equal(captured$warnings[[1]]$missing_years$year, 2026L)
  }
})

test_that("complete requests keep results and emit no coverage warnings", {
  calls = coverage_calls()
  local_mocked_bindings(get_bls_api = coverage_api(calls, coverage_recent),
                        bls_current_year = function() 2026L)
  expect_no_warning(result <- get_bls("LNU04000000", 2024, 2025,
                                      annual_average = TRUE, bls_api_key = "fake"))
  expect_equal(result$value, c(4, 4))
  expect_equal(result$year, 2024:2025)
  expect_length(calls$requests, 2)
  expect_no_warning(suppressWarnings(
    get_bls("LNU04000000", 2024, 2026, annual_average = TRUE, bls_api_key = "fake"),
    classes = "bls_annual_average_unavailable"
  ))
})

test_that("large disjoint year gaps have bounded display and complete condition fields", {
  calls = coverage_calls()
  observations = list(GAPS = lapply(seq(2001, 2025, 2), coverage_obs))
  local_mocked_bindings(get_bls_api = coverage_api(calls, observations),
                        bls_current_year = function() 2026L)
  w = expect_warning(get_bls("GAPS", 2000, 2026, annual_average = TRUE,
                             bls_api_key = "fake"), class = "bls_years_unavailable")
  expect_equal(w$missing_years$year, seq.int(2000L, 2026L, 2L))
  expect_match(conditionMessage(w), "9 more ranges", fixed = TRUE)
})

test_that("genuine API failures retain their status and raw messages", {
  local_mocked_bindings(
    bls_current_year = function() 2026L,
    get_bls_api = function(...) process_bls_response(list(
      status = "REQUEST_FAILED_INVALID_PARAMETERS",
      message = list("A genuine invalid parameter.")
    ), "fake")
  )
  e = expect_error(get_bls("INVALID", 2024, 2027, bls_api_key = "fake"),
                    class = "bls_api_error")
  expect_equal(e$status, "REQUEST_FAILED_INVALID_PARAMETERS")
  expect_equal(e$messages, "A genuine invalid parameter.")
})
