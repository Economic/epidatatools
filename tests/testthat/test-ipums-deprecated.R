# The dl_ipums_*() functions were renamed get_ipums_*() in 2.0.0. The old names warn
# and pass their arguments on. The new functions are mocked, so no test calls IPUMS.

test_that("dl_ipums_*() are deprecated and call get_ipums_*()", {
  local_mocked_bindings(
    get_ipums_micro = function(extract) list("micro", extract),
    get_ipums_acs1 = function(years = NULL, variables, description = NULL, ...) {
      list("acs1", years, variables, description, list(...))
    },
    get_ipums_asec = function(years = NULL, variables, description = NULL, ...) {
      list("asec", years, variables, description, list(...))
    },
    get_ipums_cps = function(months = NULL, variables, description = NULL, ...) {
      list("cps", months, variables, description, list(...))
    }
  )

  lifecycle::expect_deprecated(micro <- dl_ipums_micro("an extract"))
  expect_equal(micro, list("micro", "an extract"))

  lifecycle::expect_deprecated(acs1 <- dl_ipums_acs1(2022, "AGE", data_format = "csv"))
  expect_equal(acs1, list("acs1", 2022, "AGE", NULL, list(data_format = "csv")))

  lifecycle::expect_deprecated(asec <- dl_ipums_asec(2022, "AGE", "my extract"))
  expect_equal(asec, list("asec", 2022, "AGE", "my extract", list()))

  months = as.Date("2024-01-01")
  lifecycle::expect_deprecated(cps <- dl_ipums_cps(months, "AGE"))
  expect_equal(cps, list("cps", months, "AGE", NULL, list()))
})
