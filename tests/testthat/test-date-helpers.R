test_that("parse_bea_date parses quarterly periods", {
  result = parse_bea_date(c("2024Q1", "2024Q4"))
  expect_equal(result, as.Date(c("2024-01-01", "2024-10-01")))
})

test_that("parse_bea_date parses monthly periods", {
  result = parse_bea_date(c("2024M01", "2024M12"))
  expect_equal(result, as.Date(c("2024-01-01", "2024-12-01")))
})

test_that("parse_bea_date parses annual periods", {
  result = parse_bea_date(c("2024", "2020"))
  expect_equal(result, as.Date(c("2024-01-01", "2020-01-01")))
})

test_that("parse_bea_date handles mixed frequencies", {
  result = parse_bea_date(c("2024Q1", "2024", "2024M06"))
  expect_equal(result, as.Date(c("2024-01-01", "2024-01-01", "2024-06-01")))
})

test_that("parse_bea_date returns NA for unrecognized formats", {
  result = parse_bea_date(c("garbage", "2024Q1"))
  expect_true(is.na(result[1]))
  expect_equal(result[2], as.Date("2024-01-01"))
})

test_that("parse_bea_date handles empty input", {
  result = parse_bea_date(character(0))
  expect_equal(result, as.Date(character(0)))
})

test_that("extract_period_component extracts year", {
  result = extract_period_component(c("2024Q1", "2024", "2024M06"), "year")
  expect_equal(result, c(2024L, 2024L, 2024L))
})

test_that("extract_period_component extracts quarter", {
  result = extract_period_component(c("2024Q1", "2024Q4", "2024"), "quarter")
  expect_equal(result, c(1L, 4L, NA_integer_))
})

test_that("extract_period_component extracts month", {
  result = extract_period_component(c("2024M01", "2024M12", "2024Q1"), "month")
  expect_equal(result, c(1L, 12L, NA_integer_))
})

test_that("extract_period_component returns NA for unrecognized formats", {
  result = extract_period_component(c("garbage"), "year")
  expect_equal(result, NA_integer_)
})

test_that("extract_period_component handles empty input", {
  result = extract_period_component(character(0), "year")
  expect_equal(result, integer(0))
})

test_that("parse_bls_date produces no warnings for monthly data", {
  expect_no_warning(
    parse_bls_date(
      date_frequency = rep("month", 12),
      year = rep(2020, 12),
      period = sprintf("M%02d", 1:12)
    )
  )
})

test_that("parse_bls_date works with vector date_frequency (multi-chunk)", {
  result = parse_bls_date(
    date_frequency = c("month", "month", "quarter", "quarter"),
    year = c(2020, 2020, 2021, 2021),
    period = c("M01", "M06", "Q01", "Q03")
  )
  expect_equal(result, as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-07-01")))
  expect_s3_class(result, "Date")
})

test_that("parse_bls_date handles semiyear in vector context", {
  result = parse_bls_date(
    date_frequency = c("month", "semiyear", "semiyear"),
    year = c(2020, 2021, 2021),
    period = c("M03", "S01", "S02")
  )
  expect_equal(result, as.Date(c("2020-03-01", "2021-01-01", "2021-07-01")))
  expect_s3_class(result, "Date")
})

test_that("normalize_api_frequency classifies BEA periods", {
  result = normalize_api_frequency(c("2024Q1", "2024M06", "2024"), "bea")
  expect_equal(result, c("quarter", "month", "year"))
})
