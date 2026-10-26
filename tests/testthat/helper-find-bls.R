# find_bls() tests search a small fixture catalog: 111 rows of real sample data from
# all 33 bls-index programs. It was made with bls-index's
#   uv run python scripts/fixture_catalog.py OUT_DIR
# at bls-index commit dbf72bd, and copied here from OUT_DIR/bls_index.parquet.

bls_fixture_catalog = function() {
  normalizePath(test_path("fixtures", "bls_index_fixture.parquet"), winslash = "/")
}

# A file:// URL that download.file() accepts on every platform.
file_url = function(path) {
  path = normalizePath(path, winslash = "/", mustWork = FALSE)
  paste0("file://", if (startsWith(path, "/")) "" else "/", path)
}

# Write a manifest for the fixture catalog into `dir`, with `changes` applied to its
# `catalog` entry, and return the manifest's path.
write_fixture_manifest = function(dir, ...) {
  catalog = bls_fixture_catalog()
  manifest = list(
    manifest_version = 1,
    schema_version = 1,
    tag = "fixture",
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    catalog = utils::modifyList(
      list(
        url = file_url(catalog),
        sha256 = bls_index_sha256_file(catalog),
        bytes = file.size(catalog),
        rows = 111
      ),
      list(...)
    )
  )
  path = tempfile("latest-", tmpdir = dir, fileext = ".json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE)
  path
}

# Point find_bls() at the fixture manifest and an empty cache for the calling test,
# so no test reads the real cache or the network.
local_bls_fixture = function(env = parent.frame()) {
  dir = withr::local_tempdir(.local_envir = env)
  cache = file.path(dir, "cache")
  withr::local_envvar(BLS_INDEX_MANIFEST_URL = NA, .local_envir = env)
  withr::local_options(
    bls_index.manifest_url = file_url(write_fixture_manifest(dir)),
    bls_index.cache_dir = cache,
    .local_envir = env
  )
  list(dir = dir, cache = cache)
}

set_manifest_url = function(url, env = parent.frame()) {
  withr::local_options(bls_index.manifest_url = url, .local_envir = env)
}

read_state = function(cache) {
  jsonlite::read_json(file.path(cache, "bls_index_state.json"))
}

edit_state = function(cache, ...) {
  state = utils::modifyList(read_state(cache), list(...))
  jsonlite::write_json(state, file.path(cache, "bls_index_state.json"), auto_unbox = TRUE)
}

quiet_find_bls = function(...) suppressMessages(find_bls(...))
