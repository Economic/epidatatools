# live_api() (helper-live-api.R) skips a live test when an API is unavailable and
# fails it for any other error.

test_that("live_api() skips when an API is unavailable", {
  daily_limit = function() {
    rlang::abort("daily threshold reached", class = "bls_api_error", status = "REQUEST_NOT_PROCESSED")
  }
  expect_condition(live_api(daily_limit()), class = "skip")

  http_status = function(status) {
    classes = c(paste0("http_", status), paste0("http_", status %/% 100 * 100), "http_error")
    rlang::abort(paste("HTTP", status), class = classes)
  }
  expect_condition(live_api(http_status(503)), class = "skip")
  expect_condition(live_api(http_status(429)), class = "skip")
})

test_that("live_api() fails on other errors", {
  request_failed = function() {
    rlang::abort("bad request", class = "bls_api_error", status = "REQUEST_FAILED")
  }
  expect_error(live_api(request_failed()), class = "bls_api_error")
  expect_error(live_api(rlang::abort("HTTP 404", class = c("http_404", "http_400", "http_error"))), class = "http_404")
  expect_error(live_api(stop("something else")), "something else")
  expect_equal(live_api(1 + 1), 2)
})
