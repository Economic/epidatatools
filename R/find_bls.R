# find_bls() searches the bls-index catalog of BLS series, published weekly by
# https://github.com/Economic/bls-index. It is a port of that repository's reference
# client (examples/find_bls.R), whose rules are in the "Clients" section of its
# design.md. Change the search rules there first.

bls_index_default_manifest =
  "https://github.com/Economic/bls-index/releases/latest/download/latest.json"
bls_index_columns = c(
  "series_id",
  "series_title",
  "program",
  "survey_name",
  "seasonal",
  "begin_year",
  "end_year",
  "end_period"
)

#' Find BLS Series by Search String
#'
#' Searches a catalog of every series published by the Bureau of Labor Statistics
#' (8.3 million series from 33 BLS programs) for titles and surveys matching a search
#' string. The catalog is built weekly from the BLS flat files by
#' \href{https://github.com/Economic/bls-index}{bls-index}. Use the series IDs with
#' [get_bls()].
#'
#' @section Searching:
#' Every word in `search_string` must match, ignoring case, in the series title, the
#' survey name or a survey alias (such as "CPI" or "JOLTS"). Words match by the part
#' they share with their English stem, so "nurses" also finds "nursing" and
#' "industries" also finds "industry". For words ending in "y" or "ies" whose stem
#' ends in "i", the shared root must be followed by "y" or "i", so "supplies" finds
#' "supply" but not "supplemental". When the root is shorter than four letters, the
#' word must match a whole word, so "CES" does not match "services". Short "y"/"ies"
#' forms allow whole-word "y", "ie" or "ies" endings: "city" finds "cities" and
#' "movies" finds "movie", without matching "citizen" or "movement".
#'
#' Results are sorted by the number of words that match the title or a survey alias,
#' then by title length (shortest first), then by title and series ID. Short titles
#' tend to be the general series. Narrow a search with more words or `survey`.
#'
#' @section The catalog:
#' The first search downloads the catalog (about 33 MB) into
#' `tools::R_user_dir("epidatatools", "cache")` and verifies its checksum. Later
#' searches use that copy, and check for a newer catalog at most once every 7 days.
#' If the check fails, for example offline, the cached catalog is used. A catalog
#' built more than 10 days ago is reported as possibly out of date.
#'
#' Settings, for testing or mirrors:
#' * `options(bls_index.manifest_url = ...)`, or else the `BLS_INDEX_MANIFEST_URL`
#'   environment variable: the catalog manifest to read
#' * `options(bls_index.cache_dir = ...)`: where to keep the catalog
#'
#' Searching needs the arrow package, which `find_bls()` offers to install.
#'
#' @param search_string Character string of words to search for.
#' @param survey Character vector of BLS program codes to search within, such as
#'   `"ln"` (Current Population Survey) or `c("la", "ln")`, ignoring case. The default,
#'   `NULL`, searches all programs. An unknown code is an error that lists the valid
#'   codes.
#' @param seasonality Character vector of seasonal adjustment values to keep: one or
#'   more of `"SA"`, `"NSA"` and `"unknown"`, ignoring case. The default, `NULL`,
#'   keeps all.
#' @param limit Maximum number of rows to return (default: 1000). Use `Inf` for all
#'   matches. When rows are cut, a message gives the total and the matches per survey.
#' @param titles_only Logical. If `TRUE`, every word must match the series title;
#'   survey names and aliases are ignored. Useful with `survey`, as in
#'   `find_bls("all items", survey = "cu", titles_only = TRUE)`.
#' @param refresh Logical. If `TRUE`, check for a newer catalog now.
#'
#' @returns A tibble with columns `series_id`, `series_title`, `program` (the BLS
#'   program code), `survey_name`, `seasonal` (`"SA"`, `"NSA"` or `"unknown"`),
#'   `begin_year`, `end_year` and `end_period`.
#'
#' @export
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' find_bls("unemployment rate", survey = "ln", seasonality = "SA")
#'
#' find_bls("registered nurses wages")
#'
#' find_bls("all items", survey = "cu", titles_only = TRUE)
find_bls = function(
  search_string,
  survey = NULL,
  seasonality = NULL,
  limit = 1000,
  titles_only = FALSE,
  refresh = FALSE
) {
  rlang::check_installed("arrow", reason = "to search the BLS catalog.")
  # Check arguments before reading the catalog, which can mean a 33 MB download
  words = bls_index_search_words(search_string)
  stopifnot(
    "`limit` must be a whole number of at least 1, or `Inf`" = is.numeric(limit) &&
      length(limit) == 1 && !is.na(limit) && limit >= 1 &&
      (is.infinite(limit) || limit == round(limit)),
    "`titles_only` must be `TRUE` or `FALSE`" = rlang::is_bool(titles_only),
    "`refresh` must be `TRUE` or `FALSE`" = rlang::is_bool(refresh),
    "`survey` must be a character vector" = is.null(survey) || is.character(survey)
  )
  if (!is.null(seasonality)) {
    seasonality = bls_index_check_values(
      seasonality,
      c("SA", "NSA", "unknown"),
      "seasonality"
    )
  }
  ds = arrow::open_dataset(bls_index_catalog(refresh))

  # Survey names and aliases are fixed per program, so match them against the small
  # program table here and scan only the titles in the catalog. A word matched by the
  # survey name lets a row in; a word matched by the title or an alias also counts
  # toward the row's rank (`.hits`).
  programs = bls_index_program_table(ds)
  aliases = dplyr::coalesce(programs$survey_aliases, "")
  query = ds
  hits = list()
  for (word in words) {
    p = bls_index_word_pattern(word)
    in_title = rlang::expr(grepl(!!p, series_title, ignore.case = TRUE))
    if (titles_only) {
      query = query |> dplyr::filter(!!in_title)
      next
    }
    by_alias = programs$program[grepl(p, aliases, ignore.case = TRUE, perl = TRUE)]
    by_name = programs$program[
      grepl(p, programs$survey_name, ignore.case = TRUE, perl = TRUE)
    ]
    query = query |>
      dplyr::filter(!!in_title | program %in% !!union(by_alias, by_name))
    hits = c(hits, list(rlang::expr(!!in_title | program %in% !!by_alias)))
  }
  if (!is.null(survey)) {
    codes = bls_index_check_values(survey, sort(programs$program), "survey")
    query = query |> dplyr::filter(program %in% !!codes)
  }
  if (!is.null(seasonality)) {
    query = query |> dplyr::filter(seasonal %in% !!seasonality)
  }
  hit_count = if (titles_only) {
    # Every row left matched every word in its title, so the count is known; testing
    # the titles again would repeat each word's regex on every match
    length(words)
  } else {
    Reduce(
      function(a, b) rlang::expr(!!a + !!b),
      lapply(hits, function(h) rlang::expr(as.integer(!!h)))
    )
  }

  # Scan the catalog once, keeping the matches in arrow memory (not R). Counts by
  # program, rank and title length show where the first `limit` rows end, so only those
  # rows (plus ties) are brought into R. survey_name is added back from the program table.
  matches = query |>
    dplyr::mutate(.hits = !!hit_count) |>
    dplyr::select(dplyr::all_of(c(setdiff(bls_index_columns, "survey_name"), ".hits"))) |>
    dplyr::compute()
  tally = matches |>
    dplyr::mutate(length = nchar(series_title)) |>
    dplyr::count(program, .hits, length) |>
    dplyr::collect()
  total = sum(tally$n)
  if (is.finite(limit) && total > limit) {
    ranks = tally |>
      dplyr::count(.hits, length, wt = n) |>
      dplyr::arrange(dplyr::desc(.hits), length)
    last = ranks[which(cumsum(ranks$n) >= limit)[1], ]
    matches = matches |>
      dplyr::filter(
        .hits > !!last$.hits |
          (.hits == !!last$.hits & nchar(series_title) <= !!last$length)
      )
  }
  result = matches |>
    dplyr::collect() |>
    dplyr::left_join(
      dplyr::select(programs, program, survey_name),
      by = "program"
    )

  # Most matched words first, then title length, then lowercased title, then series ID.
  # "radix" compares strings by bytes, so the order is the same on every machine.
  result = result[
    order(
      -result$.hits,
      nchar(result$series_title),
      tolower(result$series_title),
      result$series_id,
      method = "radix"
    ),
    bls_index_columns
  ]
  if (is.finite(limit)) {
    result = utils::head(result, limit)
  }
  result = tibble::as_tibble(result)

  if (total > nrow(result)) {
    counts = tally |>
      dplyr::count(program, wt = n) |>
      dplyr::arrange(dplyr::desc(n), program)
    rlang::inform(paste0(
      bls_index_format_count(total),
      " matches; showing the first ",
      bls_index_format_count(nrow(result)),
      ". Narrow the search, or use `limit = Inf`.\nBy survey: ",
      paste(counts$program, bls_index_format_count(counts$n), collapse = ", "),
      "."
    ))
  }
  result
}

# -- search terms -------------------------------------------------------------------

bls_index_search_words = function(search_string) {
  stopifnot(
    is.character(search_string),
    length(search_string) == 1,
    !is.na(search_string)
  )
  words = strsplit(tolower(search_string), "\\s+")[[1]]
  words = gsub("^[[:punct:]]+|[[:punct:]]+$", "", words)
  words = words[words != ""]
  if (length(words) == 0) {
    rlang::abort("`search_string` must contain at least one word.")
  }
  words
}

# For "-y" and "-ies" words, drop a final "i" from the stem before taking the shared
# root, so "industry" and "industries" both give "industr". Other endings keep the
# stem intact ("taxis" -> "taxi"). Keep a "y" or "i" after long roots so "cookies"
# does not match "cooks". Roots under 4 characters match whole words only, including
# "y", "ie" and "ies" endings for these forms ("city"/"cities", "movie"/"movies").
bls_index_word_pattern = function(word) {
  stem = SnowballC::wordStem(word, language = "english")
  y_ies = grepl("i$", stem) && grepl("(y|ies)$", word)
  root_stem = if (y_ies) sub("i$", "", stem) else stem
  root = bls_index_shared_prefix(word, root_stem)
  if (nchar(root) >= 4) {
    paste0("\\b", bls_index_escape_regex(root), if (y_ies) "(y|i)" else "")
  } else if (y_ies) {
    paste0("\\b", bls_index_escape_regex(root), "(y|ie|ies)\\b")
  } else {
    paste0("\\b", bls_index_escape_regex(word), "\\b")
  }
}

bls_index_shared_prefix = function(a, b) {
  a = strsplit(a, "")[[1]]
  b = strsplit(b, "")[[1]]
  n = min(length(a), length(b))
  same = a[seq_len(n)] == b[seq_len(n)]
  k = if (all(same)) n else which(!same)[1] - 1
  paste(a[seq_len(k)], collapse = "")
}

bls_index_escape_regex = function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

bls_index_check_values = function(x, valid, arg) {
  stopifnot(is.character(x), length(x) >= 1)
  matched = valid[match(tolower(x), tolower(valid))]
  if (anyNA(matched)) {
    rlang::abort(paste0(
      "Unknown `",
      arg,
      "` value(s): ",
      paste(x[is.na(matched)], collapse = ", "),
      ". Valid values: ",
      paste(valid, collapse = ", "),
      "."
    ))
  }
  unique(matched)
}

# The program table (code, survey name, aliases) is read once per catalog per session.
bls_index_programs = new.env()
bls_index_program_table = function(ds) {
  key = paste(ds$files, collapse = "|")
  if (is.null(bls_index_programs[[key]])) {
    bls_index_programs[[key]] = ds |>
      dplyr::distinct(program, survey_name, survey_aliases) |>
      dplyr::collect()
  }
  bls_index_programs[[key]]
}

bls_index_format_count = function(n) {
  format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# -- cached catalog -----------------------------------------------------------------

# Return the path of a verified local catalog, checking for a new one at most every 7 days.
bls_index_catalog = function(refresh = FALSE) {
  dir = getOption(
    "bls_index.cache_dir",
    tools::R_user_dir("epidatatools", "cache")
  )
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  state_file = file.path(dir, "bls_index_state.json")
  state = bls_index_read_state(state_file)
  cached = if (is.null(state)) NA_character_ else file.path(dir, state$file)
  have = !is.na(cached) && file.exists(cached)

  if (have && !refresh && isTRUE(bls_index_days_since(state$checked_at) < 7)) {
    return(bls_index_warn_if_stale(cached, state))
  }
  manifest = tryCatch(bls_index_read_manifest(), error = function(e) e)
  if (inherits(manifest, "error")) {
    if (!have) {
      rlang::abort(paste0(
        "Could not read the BLS catalog manifest: ",
        conditionMessage(manifest),
        ". If no catalog has been published yet, point ",
        "`options(bls_index.manifest_url = ...)` at a local manifest."
      ))
    }
    rlang::inform(paste0(
      "Could not check for a newer BLS catalog (",
      conditionMessage(manifest),
      "); using the cached one built ",
      state$built_at,
      "."
    ))
    bls_index_record_check(state, state_file)
    return(bls_index_warn_if_stale(cached, state))
  }

  catalog = manifest$catalog
  if (!have || !identical(state$sha256, catalog$sha256)) {
    file = paste0("bls_index-", catalog$sha256, ".parquet")
    partial = file.path(dir, paste0(file, ".part"))
    on.exit(unlink(partial), add = TRUE)
    fetched = tryCatch(
      {
        bls_index_download(catalog$url, partial)
        actual = bls_index_sha256_file(partial)
        if (!identical(actual, catalog$sha256)) {
          rlang::abort(paste0(
            "the downloaded catalog failed its checksum (expected ",
            catalog$sha256,
            ", got ",
            actual,
            ")"
          ))
        }
        TRUE
      },
      error = function(e) e
    )
    if (inherits(fetched, "error")) {
      if (!have) {
        rlang::abort(paste0(
          "Could not get the BLS catalog: ",
          conditionMessage(fetched),
          "."
        ))
      }
      rlang::inform(paste0(
        "Could not get the newer BLS catalog (",
        conditionMessage(fetched),
        "); using the cached one built ",
        state$built_at,
        "."
      ))
      bls_index_record_check(state, state_file)
      return(bls_index_warn_if_stale(cached, state))
    }
    # Delete the old catalog only once the new one is in place
    if (!bls_index_rename(partial, file.path(dir, file))) {
      if (!have) {
        rlang::abort(paste0("Could not save the BLS catalog to ", dir, "."))
      }
      rlang::inform(paste0(
        "Could not save the newer BLS catalog to ",
        dir,
        "; using the cached one built ",
        state$built_at,
        "."
      ))
      bls_index_record_check(state, state_file)
      return(bls_index_warn_if_stale(cached, state))
    }
    if (have && basename(cached) != file) {
      unlink(cached)
    }
    cached = file.path(dir, file)
  }
  state = list(
    file = basename(cached),
    sha256 = catalog$sha256,
    tag = manifest$tag,
    built_at = manifest$built_at,
    checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  bls_index_write_state(state, state_file)
  bls_index_warn_if_stale(cached, state)
}

# A separate function so tests can make the rename fail. Callers report a failure
# themselves, so file.rename()'s own warning is dropped.
bls_index_rename = function(from, to) suppressWarnings(file.rename(from, to))

# Read the state file, treating one that is unreadable or incomplete (for example
# after an interrupted write) as missing, so the catalog is fetched again.
bls_index_read_state = function(state_file) {
  if (!file.exists(state_file)) {
    return(NULL)
  }
  state = tryCatch(
    suppressWarnings(jsonlite::read_json(state_file)),
    error = function(e) NULL
  )
  is_string = function(x) is.character(x) && length(x) == 1 && !is.na(x)
  fields = c("file", "sha256", "built_at", "checked_at")
  if (!is.list(state) || !all(vapply(state[fields], is_string, logical(1)))) {
    return(NULL)
  }
  state
}

# Best effort: an unwritable cache must not stop a search that has a cached catalog.
# Writing to a temporary file and renaming it means a reader never sees a partial file.
bls_index_write_state = function(state, state_file) {
  temp = tempfile("bls_index_state-", tmpdir = dirname(state_file), fileext = ".json")
  on.exit(unlink(temp))
  try(
    {
      suppressWarnings(
        jsonlite::write_json(state, temp, auto_unbox = TRUE, pretty = TRUE)
      )
      bls_index_rename(temp, state_file)
    },
    silent = TRUE
  )
  invisible(NULL)
}

# Record a check attempt, keeping the cached catalog's details. A failed check counts
# too, so searches do not retry the network every time; `refresh = TRUE` retries now.
bls_index_record_check = function(state, state_file) {
  state$checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  bls_index_write_state(state, state_file)
}

# The option takes precedence over the environment variable; an empty variable is unset.
bls_index_manifest_url = function() {
  url = getOption("bls_index.manifest_url")
  if (is.null(url)) {
    url = Sys.getenv("BLS_INDEX_MANIFEST_URL")
  }
  if (!nzchar(url)) {
    url = bls_index_default_manifest
  }
  url
}

bls_index_read_manifest = function() {
  url = bls_index_manifest_url()
  path = tempfile(fileext = ".json")
  on.exit(unlink(path))
  bls_index_download(url, path)
  jsonlite::read_json(path)
}

bls_index_download = function(url, dest) {
  # A 33 MB catalog can take more than R's default 60 s on a slow connection.
  old = options(timeout = max(900, getOption("timeout")))
  on.exit(options(old))
  user_agent = paste0(
    "epidatatools/",
    utils::packageVersion("epidatatools"),
    " find_bls (https://github.com/Economic/epidatatools)"
  )
  # download.file reports a failure as warnings plus an error; keep one short error.
  status = tryCatch(
    suppressWarnings(utils::download.file(
      url,
      dest,
      mode = "wb",
      quiet = TRUE,
      headers = c("User-Agent" = user_agent)
    )),
    error = function(e) e
  )
  if (inherits(status, "error") || !identical(status, 0L)) {
    rlang::abort(paste0("could not download ", url))
  }
}

bls_index_sha256_file = function(path) {
  con = file(path, "rb")
  on.exit(close(con))
  as.vector(as.character(openssl::sha256(con))) # as.vector drops the "hash" class
}

bls_index_days_since = function(timestamp) {
  as.numeric(difftime(
    Sys.time(),
    as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    units = "days"
  ))
}

bls_index_warn_if_stale = function(path, state) {
  age = bls_index_days_since(state$built_at)
  if (!is.na(age) && age > 10) {
    rlang::inform(paste0(
      "The BLS catalog was built ",
      floor(age),
      " days ago and may be out of date. ",
      "Use `refresh = TRUE` to check for a newer one."
    ))
  }
  path
}
