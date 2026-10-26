# interpolated_quantile(), interpolated_median() and binipolate() were deprecated in
# 2.0.0 in favor of averaged_quantile() and averaged_median(). They still return what
# they did before; the expected values below were recorded before the deprecation.

test_that("the interpolated functions are deprecated", {
  lifecycle::expect_deprecated(interpolated_quantile(mtcars$mpg, bin_size = 0.5))
  lifecycle::expect_deprecated(interpolated_median(mtcars$mpg, bin_size = 0.5))
  lifecycle::expect_deprecated(binipolate(mtcars, mpg, bin_size = 0.25))
})

test_that("the interpolated functions return the same results as before", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    interpolated_quantile(mtcars$mpg, bin_size = 0.5, probs = c(0.25, 0.5, 0.75)),
    c(15.25, 19, 22.5)
  )
  expect_equal(interpolated_median(mtcars$mpg, bin_size = 0.5), 19)

  binned = binipolate(
    mtcars,
    mpg,
    probs = c(0.25, 0.75),
    bin_size = 0.25,
    .by = cyl,
    w = wt
  )
  expect_equal(binned$cyl, c(4, 4, 6, 6, 8, 8))
  expect_equal(binned$probs, rep(c(0.25, 0.75), 3))
  expect_equal(
    binned$value,
    c(21.8628313528336, 28.2864649936061, 18.0205924855491,
      20.6154458598726, 13.004931640625, 16.0754453316953)
  )
})

test_that("each call warns once, not again for the functions it uses", {
  # "warning" warns on every call instead of once every 8 hours
  rlang::local_options(lifecycle_verbosity = "warning")
  count_deprecations = function(expr) {
    n = 0
    withCallingHandlers(
      expr,
      lifecycle_warning_deprecated = function(w) {
        n <<- n + 1
        invokeRestart("muffleWarning")
      }
    )
    n
  }

  expect_equal(count_deprecations(interpolated_median(mtcars$mpg, bin_size = 0.5)), 1)
  expect_equal(count_deprecations(binipolate(mtcars, mpg, bin_size = 0.25)), 1)
})

test_that("the binipolate() warning shows how to use averaged_quantile()", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    binipolate(mtcars, mpg, bin_size = 0.25),
    "reframe",
    class = "lifecycle_warning_deprecated"
  )
})
