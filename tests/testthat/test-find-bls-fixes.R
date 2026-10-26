# Regression tests for the review of #29. Search rules are in the "Clients" section of
# bls-index's design.md; these tests cover where find_bls() departs from or adds to it.

# -- word patterns ------------------------------------------------------------------

test_that("plurals ending in -ies match their singular, and the reverse", {
  # SnowballC stems both "industry" and "industries" to "industri"
  expect_identical(bls_index_word_pattern("industries"), bls_index_word_pattern("industry"))
  expect_identical(bls_index_word_pattern("salaries"), bls_index_word_pattern("salary"))
  expect_true(grepl(bls_index_word_pattern("industries"), "Total private industry", perl = TRUE))
})

test_that("short -y words match their -ies plural as whole words", {
  expect_identical(bls_index_word_pattern("cities"), bls_index_word_pattern("city"))
  p = bls_index_word_pattern("city")
  expect_true(grepl(p, "U.S. city average", perl = TRUE))
  expect_true(grepl(p, "large cities", perl = TRUE))
  expect_false(grepl(p, "citizens", perl = TRUE))
})

test_that("short words still match whole words only", {
  p = bls_index_word_pattern("ces")
  expect_true(grepl(p, "CES data", perl = TRUE, ignore.case = TRUE))
  expect_false(grepl(p, "services", perl = TRUE, ignore.case = TRUE))
  expect_identical(bls_index_word_pattern("nurses"), "\\bnurs")
})

test_that("on the fixture, singular and plural searches find the same titles", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  expect_identical(
    quiet_find_bls("industries", titles_only = TRUE)$series_id,
    quiet_find_bls("industry", titles_only = TRUE)$series_id
  )
  expect_identical(
    quiet_find_bls("cities", titles_only = TRUE)$series_id,
    quiet_find_bls("city", titles_only = TRUE)$series_id
  )
  expect_gt(nrow(quiet_find_bls("cities", titles_only = TRUE)), 0)
})

# -- arguments ----------------------------------------------------------------------

test_that("arguments are checked before the catalog is read", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  # An unreachable manifest and an empty cache: reading the catalog would fail first
  set_manifest_url("file:///nonexistent/latest.json")
  expect_error(find_bls("wage", seasonality = "X"), "Unknown `seasonality`")
  expect_error(find_bls("wage", refresh = NA), "refresh")
  expect_error(find_bls("wage", limit = 2.5), "whole number")
  expect_error(find_bls("wage", limit = NA), "whole number")
})

# -- settings -----------------------------------------------------------------------

test_that("the manifest option takes precedence over the environment variable", {
  withr::local_options(bls_index.manifest_url = "file:///from-option/latest.json")
  withr::local_envvar(BLS_INDEX_MANIFEST_URL = "file:///from-envvar/latest.json")
  expect_identical(bls_index_manifest_url(), "file:///from-option/latest.json")
})

test_that("an empty environment variable counts as unset", {
  withr::local_options(bls_index.manifest_url = NULL)
  withr::local_envvar(BLS_INDEX_MANIFEST_URL = "")
  expect_identical(bls_index_manifest_url(), bls_index_default_manifest)
  withr::local_envvar(BLS_INDEX_MANIFEST_URL = "file:///from-envvar/latest.json")
  expect_identical(bls_index_manifest_url(), "file:///from-envvar/latest.json")
})

# -- cache --------------------------------------------------------------------------

test_that("a damaged state file is treated as missing", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  state_file = file.path(fixture$cache, "bls_index_state.json")
  for (damaged in c('{"file": "bls_in', "{}", '{"file": 1}')) {
    writeLines(damaged, state_file)
    expect_gt(nrow(quiet_find_bls("wage")), 0)
    expect_true(is.character(read_state(fixture$cache)$sha256))
  }
})

test_that("an unparseable checked_at counts as due for a check", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  edit_state(fixture$cache, checked_at = "not a time")
  expect_gt(nrow(quiet_find_bls("wage")), 0)
})

test_that("an unwritable state file does not stop a search", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  # A directory where the state file goes cannot be read or written as a file
  dir.create(file.path(fixture$cache, "bls_index_state.json"), recursive = TRUE)
  expect_no_warning(first <- quiet_find_bls("wage"))
  expect_gt(nrow(first), 0)
  expect_no_warning(again <- quiet_find_bls("wage"))
  expect_gt(nrow(again), 0)
})

test_that("the state file is written whole, leaving no temporary files", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  quiet_find_bls("wage", refresh = TRUE)
  expect_setequal(
    grep("state", list.files(fixture$cache), value = TRUE),
    "bls_index_state.json"
  )
})

test_that("a failed rename keeps the cached catalog", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  quiet_find_bls("wage")
  old = file.path(fixture$cache, read_state(fixture$cache)$file)

  # A newer catalog (same bytes, different checksum in the manifest) that downloads
  # and verifies, but cannot be moved into place
  local_mocked_bindings(bls_index_rename = function(from, to) FALSE)
  newer = write_fixture_manifest(fixture$dir)
  set_manifest_url(file_url(newer))
  edit_state(fixture$cache, sha256 = strrep("2", 64), checked_at = "2000-01-01T00:00:00Z")

  expect_message(find_bls("wage"), "Could not save the newer BLS catalog")
  expect_true(file.exists(old))
  expect_gt(nrow(quiet_find_bls("wage", refresh = TRUE)), 0)
})

test_that("a failed rename with no cached catalog is an error", {
  skip_if_not_installed("arrow")
  local_bls_fixture()
  local_mocked_bindings(bls_index_rename = function(from, to) FALSE)
  expect_error(quiet_find_bls("wage"), "Could not save the BLS catalog")
})
