# Fake get_bls_api(): one monthly (January) observation per year for each
# series, and a catalog when requested
fake_bls_api = function(calls) {
  function(series_ids, start, end, catalog = FALSE, bls_api_key = "") {
    calls$n = calls$n + 1
    calls$sizes = c(calls$sizes, length(series_ids))
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
      if (catalog) {
        series$catalog = list(series_title = paste("Title", id), series_id = id)
      }
      series
    })
  }
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
  calls = new.env()
  calls$n = 0
  local_mocked_bindings(get_bls_api = fake_bls_api(calls))

  ids = sprintf("S%03d", 60:1)
  result = get_bls(
    c(first = ids[1], ids[-1]),
    start = 1980,
    end = 2024,
    bls_api_key = "fake"
  )

  # 2 batches of series x 3 spans of years
  expect_equal(calls$n, 6)
  expect_equal(sort(unique(calls$sizes)), c(10, 50))
  expect_equal(nrow(result), 60 * 45)
  expect_equal(nrow(dplyr::distinct(result, series_id, date)), 60 * 45)
  expect_equal(unique(result$series_id), ids)
  expect_equal(result$name[result$series_id == ids[1]][1], "first")
  expect_equal(unique(result$series_title), paste("Title", ids))
  expect_equal(range(result$year), c(1980, 2024))
})
