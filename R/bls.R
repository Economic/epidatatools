#' Handle BLS API error responses
#'
#' Checks the status of a BLS API response and throws structured error if the
#' request did not succeed
#'
#' @param result Parsed JSON response from the BLS API
#' @noRd
handle_bls_api_error = function(result) {
  status = result$status %||% "UNKNOWN_STATUS"

  if (status != "REQUEST_SUCCEEDED") {
    messages = as.character(unlist(result$message))

    error_msg = paste0("BLS API error (", status, ")")
    if (length(messages) > 0) {
      error_msg = paste0(error_msg, ": ", paste(messages, collapse = "\n"))
    }

    rlang::abort(
      error_msg,
      class = "bls_api_error",
      status = status,
      messages = messages
    )
  }
}

#' Make a single BLS API timeseries request
#'
#' @param series_ids Character vector of at most 50 BLS series IDs
#' @param start Start year
#' @param end End year, at most 19 years after `start`
#' @param catalog Request series catalog metadata
#' @param bls_api_key BLS API key (defaults to BLS_API_KEY environment variable)
#'
#' @returns List of series from the API response, each with seriesID, data,
#'   and (when requested and available) catalog elements
#' @noRd
get_bls_api = function(
  series_ids,
  start,
  end,
  catalog = FALSE,
  bls_api_key = Sys.getenv("BLS_API_KEY")
) {
  # Check for API key
  validate_api_key(
    bls_api_key,
    "BLS",
    "https://data.bls.gov/registrationEngine/"
  )

  # seriesid must be a JSON array, even for a single series
  body = list(
    seriesid = as.list(unname(series_ids)),
    startyear = as.character(start),
    endyear = as.character(end),
    catalog = catalog,
    registrationkey = bls_api_key
  )

  # Make API request
  response = httr::POST(
    "https://api.bls.gov/publicAPI/v2/timeseries/data/",
    body = body,
    encode = "json"
  )

  httr::stop_for_status(response)

  # Parse JSON response
  content = httr::content(response, as = "text", encoding = "UTF-8")
  result = jsonlite::fromJSON(content, simplifyVector = FALSE)

  # Check for API errors
  handle_bls_api_error(result)

  # Pass along non-fatal notes, such as missing catalog data
  purrr::walk(as.character(unlist(result$message)), rlang::inform)

  extract_bls_series(result)
}

#' Extract the series from a successful BLS API response
#'
#' @param result Parsed JSON response from the BLS API
#' @returns List of series
#' @noRd
extract_bls_series = function(result) {
  series = result$Results$series

  # Fail loudly rather than silently returning no data if the response shape
  # ever changes
  if (is.null(series)) {
    rlang::abort(
      "BLS API error: response did not contain the expected `Results$series` element",
      class = "bls_api_error",
      status = result$status,
      messages = as.character(unlist(result$message))
    )
  }

  series
}

#' Split years into spans the BLS API accepts in one request
#'
#' @param start Start year
#' @param end End year
#' @param year_limit Maximum number of years per request
#'
#' @returns List of c(start, end) pairs
#' @noRd
bls_year_spans = function(start, end, year_limit = 20) {
  span_starts = seq(start, end, by = year_limit)
  span_ends = pmin(span_starts + year_limit - 1, end)
  purrr::map2(span_starts, span_ends, c)
}

#' Split series IDs into batches the BLS API accepts in one request
#'
#' @param series_ids Character vector of BLS series IDs
#' @param series_limit Maximum number of series per request
#'
#' @returns List of character vectors
#' @noRd
bls_series_batches = function(series_ids, series_limit = 50) {
  unname(split(series_ids, ceiling(seq_along(series_ids) / series_limit)))
}

#' Retrieve BLS series across as many API requests as needed
#'
#' Splits the request into batches of series and spans of years, then combines
#' the results so each series has all of its data and its catalog.
#'
#' @param series Character vector of BLS series IDs
#' @param start Start year
#' @param end End year
#' @param catalog Request series catalog metadata
#' @param bls_api_key BLS API key
#'
#' @returns List of series named by series ID, in the order given in `series`
#' @noRd
get_bls_payload = function(series, start, end, catalog, bls_api_key) {
  series_ids = unique(unname(series))

  requests = tidyr::expand_grid(
    ids = bls_series_batches(series_ids),
    years = bls_year_spans(start, end)
  )

  # Base Map() rather than purrr::map2(), which would wrap a bls_api_error in
  # purrr_error_indexed and hide its class and fields from callers
  responses = Map(
    function(ids, years) get_bls_api(ids, years[1], years[2], catalog, bls_api_key),
    requests$ids,
    requests$years
  ) |>
    purrr::list_flatten()

  response_ids = purrr::map_chr(responses, "seriesID")

  series_ids |>
    purrr::set_names() |>
    purrr::map(function(id) {
      matches = responses[response_ids == id]
      list(
        seriesID = id,
        catalog = purrr::detect(purrr::map(matches, "catalog"), Negate(is.null)),
        data = purrr::list_flatten(purrr::map(matches, "data"))
      )
    })
}

#' Retrieve BLS catalog metadata without letting catalog failures block data
#'
#' The BLS API sometimes rejects any request that asks for catalog data and
#' includes certain series (`REQUEST_FAILED`), even though the same request
#' succeeds without the catalog. Data is therefore requested separately, and
#' this function requests catalogs on their own, splitting a failing batch in
#' half until the series that fail are isolated.
#'
#' @param series_ids Character vector of BLS series IDs
#' @param year Year to request; catalog data does not depend on the year
#' @param bls_api_key BLS API key
#'
#' @returns List with `catalogs` (named by series ID; NULL where unavailable)
#'   and `failed` (IDs whose catalog request failed)
#' @noRd
get_bls_catalogs = function(series_ids, year, bls_api_key) {
  # Base lapply() so a bls_api_error reaches callers unwrapped (see
  # get_bls_payload())
  results = series_ids |>
    bls_series_batches() |>
    lapply(get_bls_catalog_batch, year = year, bls_api_key = bls_api_key)

  list(
    catalogs = purrr::list_flatten(purrr::map(results, "catalogs")),
    failed = purrr::list_c(purrr::map(results, "failed"), ptype = character())
  )
}

#' @noRd
get_bls_catalog_batch = function(series_ids, year, bls_api_key) {
  # The data requests already pass along BLS's notes (such as "Unable to get
  # Catalog Data"), which BLS repeats on catalog requests
  result = tryCatch(
    suppressMessages(
      get_bls_api(series_ids, year, year, catalog = TRUE, bls_api_key = bls_api_key)
    ),
    bls_api_error = function(e) {
      # Only REQUEST_FAILED is worth splitting; anything else (such as an
      # exceeded daily limit) would fail again, so stop now
      if (!identical(e$status, "REQUEST_FAILED")) {
        stop(e)
      }
      e
    }
  )

  if (!inherits(result, "bls_api_error")) {
    catalogs = series_ids |>
      purrr::set_names() |>
      purrr::map(~ purrr::detect(result, function(s) identical(s$seriesID, .x))$catalog)
    return(list(catalogs = catalogs, failed = character()))
  }

  if (length(series_ids) == 1) {
    return(list(
      catalogs = purrr::set_names(list(NULL), series_ids),
      failed = series_ids
    ))
  }

  half = ceiling(length(series_ids) / 2)
  first = get_bls_catalog_batch(series_ids[seq_len(half)], year, bls_api_key)
  second = get_bls_catalog_batch(series_ids[-seq_len(half)], year, bls_api_key)

  list(
    catalogs = c(first$catalogs, second$catalogs),
    failed = c(first$failed, second$failed)
  )
}

#' @noRd
warn_bls_catalog_unavailable = function(failed) {
  rlang::warn(
    c(
      paste0(
        "BLS could not return catalog metadata for ",
        length(failed),
        " series, so `series_title` and metadata are missing for them."
      ),
      i = paste("Series:", paste(failed, collapse = ", ")),
      i = "Their data was retrieved. Catalog failures are usually temporary; try again later."
    ),
    class = "bls_catalog_unavailable",
    series_ids = failed
  )
}

#' Retrieve data from the BLS API
#'
#' Requires you to have an \href{https://www.bls.gov/developers/}{BLS API key} saved in the `BLS_API_KEY` environment variable.
#'
#' @param series BLS series code
#' @param start Start year (numeric)
#' @param end End year (numeric)
#' @param metadata Flag for additional metadata
#' @param bls_api_key BLS API key (defaults to BLS_API_KEY environment variable)
#'
#' @returns a tibble
#'
#' @details
#' Series titles and other metadata come from the BLS series catalog, which is
#' requested separately from the data. If BLS cannot return the catalog for
#' some series, their data is still returned, but `series_title` is `NA` and
#' their metadata is missing. A warning of class `bls_catalog_unavailable`
#' lists those series. Series that BLS has no catalog for get `NA` titles
#' without a warning.
#'
#' When BLS has no data for an observation, it sends a value that is not a number,
#' usually with a footnote that explains why. For example, most series have no
#' October 2025 value because of the 2025 lapse in appropriations. These values are
#' `NA`, and a warning of class `bls_missing_values` lists them with BLS's
#' explanation. Turn it off with `options(epidatatools.warn_missing = FALSE)`, or for
#' one call with `suppressWarnings(get_bls(...), classes = "bls_missing_values")`.
#' A value that is not a number and has no footnote gives a separate warning of class
#' `bls_unexpected_values`, which the option does not turn off.
#'
#' With `metadata = TRUE`, each series' `data` has numeric values and a `footnotes`
#' column with BLS's footnotes for each observation.
#'
#' @export
#' @examplesIf nzchar(Sys.getenv("BLS_API_KEY"))
#' get_bls("LNU02300060", start = 2020, end = 2024)
#'
#' bls_series_ids = c(
#'   emp_fb_2534 = "LNU02073399",
#'   epop_asianmen_2554 = "LNU02332330Q",
#'   cpi_semi = "CUUS0000SA0",
#'   "LNU02300060"
#' )
#' get_bls(bls_series_ids, start = 2024, end = 2024)
#'
#' complete_results = get_bls(bls_series_ids, start = 2024, end = 2024, metadata = TRUE)
#' complete_results
#'
#' complete_results |>
#'   tidyr::unnest(metadata)
get_bls = function(
  series,
  start,
  end,
  metadata = FALSE,
  bls_api_key = Sys.getenv("BLS_API_KEY")
) {
  # Check for API key
  validate_api_key(
    bls_api_key,
    "BLS",
    "https://data.bls.gov/registrationEngine/"
  )

  # Request data and catalogs separately, so a catalog failure cannot block
  # the data
  raw_results = get_bls_payload(
    series,
    start = start,
    end = end,
    catalog = FALSE,
    bls_api_key = bls_api_key
  )

  catalogs = get_bls_catalogs(names(raw_results), end, bls_api_key)
  raw_results = purrr::imap(raw_results, function(x, id) {
    x$catalog = catalogs$catalogs[[id]]
    x
  })

  if (length(catalogs$failed) > 0) {
    warn_bls_catalog_unavailable(catalogs$failed)
  }

  complete_results = bls_payload_to_tibble(
    raw_results,
    series,
    start,
    end
  ) |>
    bls_convert_values()

  if (metadata) {
    complete_results
  } else {
    extract_data(complete_results, bls_series_data_extractor)
  }
}

#' @noRd
bls_payload_to_tibble = function(payload, series, start, end) {
  complete_results = names(payload) |>
    purrr::map(~ bls_payload_series_to_tibble(.x, payload, start, end)) |>
    purrr::list_rbind()

  add_series_names(complete_results, series)
}

#' @noRd
bls_payload_series_to_tibble = function(id, payload, start, end) {
  catalog = purrr::pluck(payload, id, "catalog")

  if (is.null(catalog)) {
    metadata = tibble::tibble(series_title = NA_character_)
  } else {
    metadata = catalog |>
      tibble::as_tibble() |>
      dplyr::select(-dplyr::any_of("series_id"))
  }

  data = payload |>
    purrr::pluck(id, "data") |>
    purrr::map(bls_observation_to_tibble) |>
    purrr::map(add_dates_to_bls_tibble) |>
    purrr::list_rbind()

  # sort data when present
  if (length(data) > 0) {
    data_sorted = data |>
      dplyr::arrange(.data$date)
  } else {
    data_sorted = data
  }

  results = tibble::tibble(
    series_id = id,
    metadata = list(metadata),
    data = list(data_sorted)
  )

  results
}

# One row per observation. An observation's footnotes are joined into one string,
# because a list of several footnotes would otherwise make several rows.
#' @noRd
bls_observation_to_tibble = function(x) {
  notes = purrr::map_chr(x$footnotes, function(f) f$text %||% NA_character_)
  notes = notes[!is.na(notes) & nzchar(notes)]
  x$footnotes = if (length(notes) > 0) paste(notes, collapse = "; ") else NA_character_
  tibble::as_tibble(x)
}

# Convert values to numbers. BLS sends a value that is not a number, such as "-",
# when it has no data, and usually explains why in a footnote: for example, most
# series have no October 2025 value because of the 2025 lapse in appropriations.
# Those values become NA, with one warning that gives BLS's explanation.
#' @noRd
bls_convert_values = function(complete_results) {
  unreadable = purrr::map2(
    complete_results$data,
    complete_results$series_id,
    bls_unreadable_values
  ) |>
    purrr::list_rbind()

  complete_results$data = purrr::map(complete_results$data, function(data) {
    if ("value" %in% names(data)) {
      data$value = suppressWarnings(as.numeric(data$value))
    }
    data
  })

  if (!is.null(unreadable) && nrow(unreadable) > 0) {
    explained = unreadable[!is.na(unreadable$footnotes), ]
    unexplained = unreadable[is.na(unreadable$footnotes), ]
    if (nrow(explained) > 0 && isTRUE(getOption("epidatatools.warn_missing", TRUE))) {
      warn_bls_missing_values(explained)
    }
    if (nrow(unexplained) > 0) {
      warn_bls_unexpected_values(unexplained)
    }
  }

  complete_results
}

#' @noRd
bls_unreadable_values = function(data, series_id) {
  if (!"value" %in% names(data)) {
    return(NULL)
  }
  bad = is.na(suppressWarnings(as.numeric(data$value))) & !is.na(data$value)
  tibble::tibble(
    series_id = rep(series_id, sum(bad)),
    period = paste(data$periodName[bad], data$year[bad]),
    value = data$value[bad],
    footnotes = data$footnotes[bad]
  )
}

# "October 2025 for A, B, C, D, E and 3 more", one entry per period
#' @noRd
bls_describe_periods = function(rows) {
  rows |>
    split(factor(rows$period, levels = unique(rows$period))) |>
    purrr::imap_chr(function(g, period) {
      ids = unique(g$series_id)
      shown = paste(utils::head(ids, 5), collapse = ", ")
      if (length(ids) > 5) {
        shown = paste0(shown, " and ", length(ids) - 5, " more")
      } else if (length(ids) > 1) {
        shown = sub(", ([^,]+)$", " and \\1", shown)
      }
      paste(period, "for", shown)
    }) |>
    paste(collapse = "; ")
}

#' @noRd
warn_bls_missing_values = function(explained) {
  n = nrow(explained)
  groups = split(explained, factor(explained$footnotes, levels = unique(explained$footnotes)))
  bullets = purrr::imap_chr(groups, function(g, note) {
    paste0(sub("\\.$", "", note), " (", bls_describe_periods(g), ")")
  })
  rlang::warn(
    c(
      paste0(
        "BLS has no value for ", n, " observation", if (n > 1) "s",
        ", so ", if (n > 1) "they are" else "it is", " `NA`:"
      ),
      purrr::set_names(bullets, rep("*", length(bullets))),
      i = "Set `options(epidatatools.warn_missing = FALSE)` to turn off this warning."
    ),
    class = "bls_missing_values",
    observations = explained
  )
}

#' @noRd
warn_bls_unexpected_values = function(unexplained) {
  n = nrow(unexplained)
  shown = utils::head(unexplained, 5)
  bullets = paste0(
    shown$series_id, ", ", shown$period, ": ", encodeString(shown$value, quote = '"')
  )
  rlang::warn(
    c(
      paste0(
        "BLS returned ", n, " value", if (n > 1) "s", " that ",
        if (n > 1) "are" else "is", " not a number, with no footnote to explain, so ",
        if (n > 1) "they are" else "it is", " `NA`:"
      ),
      purrr::set_names(bullets, rep("*", length(bullets))),
      if (n > 5) c(i = paste(n - 5, "more not shown."))
    ),
    class = "bls_unexpected_values",
    observations = unexplained
  )
}

#' @noRd
add_dates_to_bls_tibble = function(data) {
  data |>
    dplyr::mutate(
      date_frequency = normalize_api_frequency(.data$period, "bls"),
      date = parse_api_date(
        .data$period,
        frequency = .data$date_frequency,
        year = .data$year
      )
    )
}

#' @noRd
bls_series_data_extractor = function(series_id, complete_results) {
  generic_data_extractor(
    series_id,
    complete_results,
    metadata_cols = c("name", "series_id", "series_title", "data"),
    final_cols = c(
      "name",
      "series_id",
      "series_title",
      "date_frequency",
      "date",
      "value"
    )
  )
}
