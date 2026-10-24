test_that("handle_bea_api_error() catches errors returned under Results", {
  result = list(BEAAPI = list(Results = list(Error = list(
    APIErrorCode = "4",
    APIErrorDescription = "This UserId is not active. Please activate it and try again."
  ))))

  expect_error(
    handle_bea_api_error(result),
    "This UserId is not active. Please activate it and try again. (Error code: 4)",
    fixed = TRUE,
    class = "bea_api_error"
  )
})
