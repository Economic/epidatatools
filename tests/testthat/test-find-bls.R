# Ported from bls-index's examples/check_find_bls.R. The rules are in the "Clients"
# section of bls-index's design.md.

# -- results ------------------------------------------------------------------------

test_that("returns exactly the documented columns", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_named(quiet_find_bls("consumer"), bls_index_columns)
})

test_that("words match by their stem", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_true(any(grepl("\\bconsumers\\b", quiet_find_bls("consumer")$series_title)))
  expect_true(any(grepl("\\bhours\\b", quiet_find_bls("hourly")$series_title)))
  # A stem is not always the start of its word: "city" stems to "citi".
  expect_true("CUSR0000SA0" %in% quiet_find_bls("CPI all items U.S. city average")$series_id)
})

test_that("every word is required", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  titles = quiet_find_bls("unemployment rate")$series_title
  expect_true(all(grepl("unemployment", titles, ignore.case = TRUE)))
})

test_that("short words match whole words only: 'CES' finds ce, not 'services'", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  ces = quiet_find_bls("CES")
  expect_gt(nrow(ces), 0)
  expect_true(all(ces$program == "ce"))
})

test_that("aliases and survey names find their programs", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_equal(nrow(quiet_find_bls("JOLTS")), 4)
  expect_true("cu" %in% quiet_find_bls("consumer price index")$program)
})

test_that("survey-name matches let rows in but rank below title matches", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  hours = quiet_find_bls("hours")
  expect_true(all(c("ce", "sm") %in% hours$program))
  in_title = grepl("\\bhour", hours$series_title, ignore.case = TRUE)
  sorted = order(
    -in_title,
    nchar(hours$series_title),
    tolower(hours$series_title),
    hours$series_id,
    method = "radix"
  )
  expect_identical(sorted, seq_len(nrow(hours)))
})

test_that("alias matches count toward rank: 'CPI price' puts cu rows first", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_equal(quiet_find_bls("CPI price")$program[1], "cu")
})

test_that("titles_only ignores survey names and aliases", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  hours = quiet_find_bls("hours")
  in_title = grepl("\\bhour", hours$series_title, ignore.case = TRUE)
  titles = quiet_find_bls("hours", titles_only = TRUE)
  expect_equal(nrow(titles), sum(in_title))
  expect_true(all(grepl("\\bhour", titles$series_title, ignore.case = TRUE)))

  none = quiet_find_bls("JOLTS", titles_only = TRUE)
  expect_equal(nrow(none), 0)
  expect_named(none, bls_index_columns)
})

test_that("the removed arguments are errors", {
  expect_error(find_bls("wage", max_results = 5), "unused argument")
  expect_error(find_bls("wage", metadata = TRUE), "unused argument")
})

# -- filters ------------------------------------------------------------------------

test_that("survey takes several codes, ignoring case", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  two = quiet_find_bls("rate", survey = c("LA", "fm"))
  expect_gt(nrow(two), 0)
  expect_true(all(two$program %in% c("la", "fm")))
})

test_that("an unknown survey code is an error listing the valid codes", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_error(
    quiet_find_bls("rate", survey = "cps"),
    "Unknown `survey`.*cps.*Valid values: ap"
  )
})

test_that("seasonality takes several values, ignoring case", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  sa = quiet_find_bls("consumer", seasonality = "sa")
  expect_gt(nrow(sa), 0)
  expect_true(all(sa$seasonal == "SA"))
  both = quiet_find_bls("consumer", seasonality = c("SA", "NSA"))
  expect_setequal(unique(both$seasonal), c("SA", "NSA"))
})

test_that("an unknown seasonality is an error", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_error(quiet_find_bls("rate", seasonality = "X"), "Unknown `seasonality`")
})

test_that("an empty search is an error", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_error(quiet_find_bls(" ... "), "at least one word")
})

# -- limit --------------------------------------------------------------------------

test_that("limit returns the first rows of the full sort, and says what was cut", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  all_rows = quiet_find_bls("wage", limit = Inf)
  expect_message(
    few <- find_bls("wage", limit = 3),
    paste0(nrow(all_rows), " matches; showing the first 3.*By survey: ")
  )
  expect_equal(nrow(few), 3)
  expect_identical(few$series_id, utils::head(all_rows$series_id, 3))
  expect_no_message(find_bls("wage", limit = Inf))
})

# -- cache --------------------------------------------------------------------------

test_that("the catalog is cached under its checksum", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  state = read_state(fixture$cache)
  expect_true(file.exists(
    file.path(fixture$cache, paste0("bls_index-", state$sha256, ".parquet"))
  ))
})

test_that("an unreadable manifest is skipped within 7 days, then the cache is used", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  quiet_find_bls("wage")
  set_manifest_url("file:///nonexistent/latest.json")
  expect_no_message(find_bls("wage"))
  expect_message(find_bls("wage", refresh = TRUE), "using the cached one")
})

test_that("an old catalog is reported as possibly out of date", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  edit_state(fixture$cache, built_at = "2000-01-01T00:00:00Z")
  expect_message(find_bls("wage"), "may be out of date")
})

test_that("a newer catalog that fails its checksum is not used", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  bad = write_fixture_manifest(fixture$dir, sha256 = strrep("0", 64))
  set_manifest_url(file_url(bad))
  expect_message(
    find_bls("wage", refresh = TRUE),
    "failed its checksum.*using the cached one"
  )
})

test_that("a failed download uses the cache, and counts as a check", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  gone = write_fixture_manifest(
    fixture$dir,
    sha256 = strrep("1", 64),
    url = "file:///nonexistent/bls_index.parquet"
  )
  set_manifest_url(file_url(gone))
  edit_state(fixture$cache, checked_at = "2000-01-01T00:00:00Z")
  expect_message(find_bls("wage"), "could not download.*using the cached one")
  expect_no_message(find_bls("wage"))
})

test_that("with no cache, a bad catalog or a missing manifest is an error", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  bad = write_fixture_manifest(fixture$dir, sha256 = strrep("0", 64))
  set_manifest_url(file_url(bad))
  expect_error(quiet_find_bls("wage"), "failed its checksum")
  set_manifest_url("file:///nonexistent/latest.json")
  expect_error(quiet_find_bls("wage"), "Could not read")
})
