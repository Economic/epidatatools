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
  expect_false(grepl(p, "electricity", perl = TRUE))
  expect_false(grepl(bls_index_word_pattern("cities"), "citizen", perl = TRUE))
  expect_false(grepl(bls_index_word_pattern("cities"), "electricity", perl = TRUE))
})

test_that("y/ies stemming keeps other endings and excludes unrelated words", {
  expect_identical(
    grepl(bls_index_word_pattern("taxis"), c("Taxi", "Taxis"),
      ignore.case = TRUE, perl = TRUE),
    c(TRUE, TRUE)
  )
  for (case in list(
    list(word = "cookies", titles = c("cookie", "cookies", "cooks")),
    list(word = "supplies", titles = c("supply", "supplies", "supplemental")),
    list(word = "movies", titles = c("movie", "movies", "movement"))
  )) {
    expect_identical(
      grepl(bls_index_word_pattern(case$word), case$titles, perl = TRUE),
      c(TRUE, TRUE, FALSE),
      info = case$word
    )
  }
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
  for (pair in list(c("industry", "industries"), c("salary", "salaries"), c("city", "cities"))) {
    singular = quiet_find_bls(pair[1], titles_only = TRUE, limit = Inf)
    plural = quiet_find_bls(pair[2], titles_only = TRUE, limit = Inf)
    expect_gt(nrow(singular), 0)
    expect_identical(singular, plural, info = paste(pair, collapse = "/"))
  }
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

test_that("state writes report success, write failure, and rename failure", {
  dir = withr::local_tempdir()
  state_file = file.path(dir, "state.json")
  state = list(file = "old.parquet", sha256 = "old")
  expect_true(bls_index_write_state(state, state_file))
  expect_identical(jsonlite::read_json(state_file), state)

  expect_false(bls_index_write_state(state, file.path(dir, "missing", "state.json")))
  local_mocked_bindings(bls_index_rename = function(from, to) FALSE)
  expect_false(bls_index_write_state(list(file = "new.parquet"), state_file))
  expect_identical(jsonlite::read_json(state_file), state)
  expect_identical(list.files(dir), "state.json")
})

test_that("a new catalog keeps the old cache until its state is saved", {
  skip_if_not_installed("arrow")
  fixture = local_bls_fixture()
  previous = quiet_find_bls("wage", limit = Inf)
  old_state = read_state(fixture$cache)
  old_cached = file.path(fixture$cache, old_state$file)

  # A second valid catalog with observably different results and a real checksum.
  newer = arrow::read_parquet(bls_fixture_catalog())
  newer$series_title = paste("Updated", newer$series_title)
  catalog = file.path(fixture$dir, "new.parquet")
  arrow::write_parquet(newer, catalog)
  sha = bls_index_sha256_file(catalog)
  expect_false(identical(sha, old_state$sha256))
  manifest = write_fixture_manifest(
    fixture$dir, url = file_url(catalog), sha256 = sha, bytes = file.size(catalog)
  )
  new_cached = file.path(fixture$cache, paste0("bls_index-", sha, ".parquet"))
  set_manifest_url(file_url(manifest))

  write_state = bls_index_write_state
  fail_state = TRUE
  local_mocked_bindings(bls_index_write_state = function(state, state_file) {
    if (fail_state) FALSE else write_state(state, state_file)
  })
  current = quiet_find_bls("wage", refresh = TRUE, limit = Inf)
  expect_identical(current$series_id, previous$series_id)
  expect_identical(current$series_title, paste("Updated", previous$series_title))
  expect_identical(bls_index_sha256_file(new_cached), sha)
  expect_true(file.exists(old_cached))
  expect_identical(read_state(fixture$cache), old_state)

  # A later offline call must still find the file named by the saved state.
  set_manifest_url("file:///nonexistent/latest.json")
  expect_identical(quiet_find_bls("wage", refresh = TRUE, limit = Inf), previous)
  expect_true(file.exists(old_cached))
  expect_identical(read_state(fixture$cache), old_state)

  fail_state = FALSE
  set_manifest_url(file_url(manifest))
  expect_identical(quiet_find_bls("wage", refresh = TRUE, limit = Inf), current)
  expect_identical(read_state(fixture$cache)$file, basename(new_cached))
  expect_identical(read_state(fixture$cache)$sha256, sha)
  expect_true(file.exists(new_cached))
  expect_false(file.exists(old_cached))
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
