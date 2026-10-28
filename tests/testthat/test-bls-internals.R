# Fake get_bls_api(): one monthly (January) observation per year for each
# series, and a catalog when requested. A catalog request that includes any
# series in `catalog_fails` fails with `status`, as BLS does during a catalog
# outage. Series in `no_data` have no observations but can still have a catalog.
fake_bls_api = function(calls, catalog_fails = character(), status = "REQUEST_FAILED",
                        no_data = character()) {
  function(series_ids, start, end, catalog = FALSE, bls_api_key = "",
           annual_average = FALSE) {
    calls$n = calls$n + 1
    calls$sizes = c(calls$sizes, length(series_ids))
    if (catalog) {
      calls$catalog_n = calls$catalog_n + 1
    }
    if (catalog && any(series_ids %in% catalog_fails)) {
      rlang::abort("Your request has failed.", class = "bls_api_error", status = status)
    }
    purrr::map(series_ids, function(id) {
      series = list(
        seriesID = id,
        data = purrr::map(seq(end, start), function(y) {
          list(
            year = as.character(y),
            period = "M01",
            periodName = "January",
            value = "1.5",
            footnotes = list(list())
          )
        })
      )
      if (id %in% no_data) {
        series$data = list()
      }
      if (catalog) {
        series$catalog = list(series_title = paste("Title", id), series_id = id)
      }
      series
    })
  }
}

new_calls = function() {
  calls = new.env()
  calls$n = 0
  calls$catalog_n = 0
  calls
}

test_that("bls_year_spans() splits years into spans of at most 20", {
  expect_equal(bls_year_spans(2024, 2024), list(c(2024, 2024)))
  expect_equal(bls_year_spans(2005, 2024), list(c(2005, 2024)))
  expect_equal(bls_year_spans(2004, 2024), list(c(2004, 2023), c(2024, 2024)))
  expect_equal(
    bls_year_spans(1980, 2024),
    list(c(1980, 1999), c(2000, 2019), c(2020, 2024))
  )
})

test_that("bls_series_batches() splits series into batches of at most 50", {
  ids = sprintf("S%03d", 1:120)
  expect_equal(lengths(bls_series_batches(ids[1])), 1)
  expect_equal(lengths(bls_series_batches(ids[1:50])), 50)
  expect_equal(lengths(bls_series_batches(ids[1:51])), c(50, 1))
  expect_equal(lengths(bls_series_batches(ids)), c(50, 50, 20))
  expect_equal(unlist(bls_series_batches(ids)), ids)
})

test_that("handle_bls_api_error() raises bls_api_error on failed requests", {
  result = list(
    status = "REQUEST_FAILED",
    message = list("Your request has failed.")
  )
  err = expect_error(handle_bls_api_error(result), class = "bls_api_error")
  expect_equal(err$status, "REQUEST_FAILED")
  expect_match(conditionMessage(err), "Your request has failed.", fixed = TRUE)

  expect_null(handle_bls_api_error(list(status = "REQUEST_SUCCEEDED")))
})

test_that("extract_bls_series() errors when the response has no series", {
  result = list(status = "REQUEST_SUCCEEDED", Results = list(list(series = list())))
  expect_error(extract_bls_series(result), class = "bls_api_error")
})

test_that("get_bls() combines batches and year spans without duplicates", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls))

  ids = sprintf("S%03d", 60:1)
  result = get_bls(
    c(first = ids[1], ids[-1]),
    start = 1980,
    end = 2024,
    bls_api_key = "fake"
  )

  # 2 batches of series x 3 spans of years, plus 1 catalog request per batch
  expect_equal(calls$n, 8)
  expect_equal(calls$catalog_n, 2)
  expect_equal(sort(unique(calls$sizes)), c(10, 50))
  expect_equal(nrow(result), 60 * 45)
  expect_equal(nrow(dplyr::distinct(result, series_id, date)), 60 * 45)
  expect_equal(unique(result$series_id), ids)
  expect_equal(result$name[result$series_id == ids[1]][1], "first")
  expect_equal(unique(result$series_title), paste("Title", ids))
  expect_equal(range(result$year), c(1980, 2024))
})

test_that("default get_bls() returns typed empty flat output when BLS has no data", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls, no_data = "EMPTY"))

  expect_warning(
    result <- get_bls(c(empty = "EMPTY"), 2024, 2025, bls_api_key = "fake"),
    class = "bls_years_unavailable"
  )
  expect_identical(result, tibble::tibble(
    name = character(), series_id = character(), series_title = character(),
    date_frequency = character(), date = as.Date(character()),
    year = numeric(), value = numeric()
  ))
  expect_identical(
    suppressWarnings(get_bls(c(empty = "EMPTY"), 2024, 2025, bls_api_key = "fake",
                             annual_average = FALSE), classes = "bls_years_unavailable"),
    result
  )
})

test_that("default get_bls() retains metadata and typed nested data for empty series", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls, no_data = "EMPTY"))

  expect_warning(
    result <- get_bls(c(empty = "EMPTY"), 2024, 2025, metadata = TRUE,
                      bls_api_key = "fake"),
    class = "bls_years_unavailable"
  )
  expect_identical(result$name, "empty")
  expect_identical(result$series_id, "EMPTY")
  expect_identical(result$metadata[[1]], tibble::tibble(series_title = "Title EMPTY"))
  expect_identical(result$data[[1]], tibble::tibble(
    year = character(), period = character(), periodName = character(),
    value = numeric(), footnotes = character(),
    date_frequency = character(), date = as.Date(character())
  ))
  expect_identical(
    suppressWarnings(get_bls(c(empty = "EMPTY"), 2024, 2025, metadata = TRUE,
                             bls_api_key = "fake", annual_average = FALSE),
                     classes = "bls_years_unavailable"),
    result
  )
})

test_that("empty series do not remove other series' observations in default calls", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls, no_data = "EMPTY"))

  expect_warning(
    result <- get_bls(c("EMPTY", "PRESENT"), 2024, 2025, bls_api_key = "fake"),
    class = "bls_years_unavailable"
  )
  expect_equal(result, get_bls("PRESENT", 2024, 2025, bls_api_key = "fake"))

  expect_warning(
    nested <- get_bls(c("EMPTY", "PRESENT"), 2024, 2025, metadata = TRUE,
                      bls_api_key = "fake"),
    class = "bls_years_unavailable"
  )
  expect_identical(nested$series_id, c("EMPTY", "PRESENT"))
  expect_equal(vapply(nested$data, nrow, integer(1)), c(0L, 2L))
})

test_that("get_bls() keeps data and isolates series whose catalog fails", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls, catalog_fails = "S4"))

  ids = c("S1", "S2", "S3", "S4")
  expect_warning(
    result <- get_bls(ids, start = 2024, end = 2024, bls_api_key = "fake"),
    class = "bls_catalog_unavailable"
  )

  # Data for every series, including the one whose catalog failed
  expect_equal(unique(result$series_id), ids)
  expect_equal(nrow(result), 4)
  expect_equal(result$series_title, c("Title S1", "Title S2", "Title S3", NA))

  # 1 data request; catalog requests split [S1-S4] -> [S1, S2], [S3, S4] -> [S3], [S4]
  expect_equal(calls$n, 6)
  expect_equal(calls$catalog_n, 5)
})

test_that("bls_catalog_unavailable warning lists only the failed series", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls, catalog_fails = c("S2", "S3")))

  w = expect_warning(
    get_bls(c("S1", "S2", "S3"), start = 2024, end = 2024, bls_api_key = "fake"),
    class = "bls_catalog_unavailable"
  )
  expect_equal(w$series_ids, c("S2", "S3"))
})

test_that("get_bls() returns data when every catalog request fails", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls, catalog_fails = c("S1", "S2")))

  expect_warning(
    result <- get_bls(c("S1", "S2"), start = 2024, end = 2024, bls_api_key = "fake"),
    class = "bls_catalog_unavailable"
  )
  expect_equal(unique(result$series_id), c("S1", "S2"))
  expect_true(all(is.na(result$series_title)))

  result_metadata = suppressWarnings(
    get_bls(c("S1", "S2"), start = 2024, end = 2024, metadata = TRUE, bls_api_key = "fake")
  )
  expect_equal(nrow(result_metadata), 2)
})

test_that("get_bls() stops without splitting on other API errors", {
  calls = new_calls()
  local_mocked_bindings(
    get_bls_api = fake_bls_api(calls, catalog_fails = "S1", status = "REQUEST_NOT_PROCESSED")
  )

  err = expect_error(
    get_bls(c("S1", "S2"), start = 2024, end = 2024, bls_api_key = "fake"),
    class = "bls_api_error"
  )
  expect_equal(err$status, "REQUEST_NOT_PROCESSED")
  expect_equal(calls$catalog_n, 1)
})

test_that("get_bls() gives no warning when every catalog is returned", {
  calls = new_calls()
  local_mocked_bindings(get_bls_api = fake_bls_api(calls))

  expect_no_warning(
    get_bls(c("S1", "S2"), start = 2024, end = 2024, bls_api_key = "fake")
  )
})

test_that("bls_api_error reaches callers unwrapped", {
  calls = new_calls()
  local_mocked_bindings(
    get_bls_api = fake_bls_api(calls, catalog_fails = "S1", status = "REQUEST_NOT_PROCESSED")
  )

  caught = tryCatch(
    get_bls("S1", start = 2024, end = 2024, bls_api_key = "fake"),
    bls_api_error = function(e) e$status
  )
  expect_equal(caught, "REQUEST_NOT_PROCESSED")
})

test_that("BLS messages never show the API key", {
  key = "0123456789abcdef0123456789abcdef"
  limit = list(
    status = "REQUEST_NOT_PROCESSED",
    message = list(paste(
      "Request could not be serviced, as the daily threshold for total number of",
      "requests allocated to the user with registration key", key, "has been reached."
    ))
  )
  err = expect_error(process_bls_response(limit, key), class = "bls_api_error")
  expect_false(grepl(key, conditionMessage(err), fixed = TRUE))
  expect_false(any(grepl(key, err$messages, fixed = TRUE)))
  expect_match(conditionMessage(err), "registration key *** has been reached", fixed = TRUE)

  note = list(
    status = "REQUEST_SUCCEEDED",
    message = list(paste("A note mentioning", key)),
    Results = list(series = list())
  )
  expect_message(process_bls_response(note, key), "A note mentioning \\*\\*\\*")
})
