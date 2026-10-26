# Live API tests call the real BLS and BEA APIs. They run only when
# EPIDATATOOLS_LIVE_TESTS is "true", which CI sets for its one job with API keys, so
# local checks spend none of BLS's 500 daily requests. To run them locally:
#   Sys.setenv(EPIDATATOOLS_LIVE_TESTS = "true")
skip_unless_live_api = function(key) {
  skip_if_not(
    identical(Sys.getenv("EPIDATATOOLS_LIVE_TESTS"), "true"),
    "live API tests are off (set EPIDATATOOLS_LIVE_TESTS=true to run them)"
  )
  skip_if_not(nzchar(Sys.getenv(key)), paste(key, "not set"))
}

# Run a live API call, skipping the test when the API is unavailable: BLS's daily
# request limit (REQUEST_NOT_PROCESSED), or an HTTP 429 or 5xx response, as during
# maintenance. Any other error still fails the test.
live_api = function(expr) {
  unavailable = function(e) skip(paste("API unavailable:", conditionMessage(e)))
  tryCatch(
    expr,
    bls_api_error = function(e) {
      if (identical(e$status, "REQUEST_NOT_PROCESSED")) {
        unavailable(e)
      }
      stop(e)
    },
    http_500 = unavailable,
    http_429 = unavailable
  )
}
