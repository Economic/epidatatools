#' Extract catalog data from BLS search results page
#'
#' @param page HTML page object from httr::content()
#' @param series_id The series ID to extract catalog data for
#'
#' @returns A one-row tibble containing catalog metadata fields (excludes series_id and series_title)
#' @noRd
extract_catalog_data = function(page, series_id) {
  # Find the catalog div for this series (it's hidden by default with id="catalog_SERIESID")
  catalog_div = rvest::html_nodes(
    page,
    paste0("div[id='catalog_", series_id, "']")
  )

  if (length(catalog_div) == 0) {
    # Return empty tibble if no catalog data found
    return(tibble::tibble())
  }

  # The catalog data is in a table with <th> (field names) and <td> (values)
  # Extract table rows
  table_rows = rvest::html_nodes(catalog_div[[1]], "tr")

  if (length(table_rows) == 0) {
    # Fall back to text parsing if no table structure found
    catalog_text = rvest::html_text(catalog_div[[1]], trim = TRUE)
    lines = strsplit(catalog_text, "\n")[[1]] |>
      stringr::str_trim() |>
      purrr::keep(~ nchar(.x) > 0)

    # Lines alternate: odd lines are field names, even lines are values
    catalog_list = list()
    for (i in seq(1, length(lines) - 1, by = 2)) {
      field_name = lines[i] |>
        stringr::str_trim() |>
        stringr::str_replace_all(" ", "_") |>
        stringr::str_to_lower()

      field_value = lines[i + 1] |>
        stringr::str_trim()

      catalog_list[[field_name]] = field_value
    }

    if (length(catalog_list) == 0) {
      result = tibble::tibble()
    } else {
      result = tibble::as_tibble(catalog_list)
    }
    # Remove series_id and series_title columns if present
    return(
      result |> dplyr::select(-dplyr::any_of(c("series_id", "series_title")))
    )
  }

  # Parse table rows (preferred method)
  catalog_list = list()
  for (row in table_rows) {
    # Get <th> for field name and <td> for value
    th = rvest::html_node(row, "th")
    td = rvest::html_node(row, "td")

    if (!is.na(th) && !is.na(td)) {
      field_name = rvest::html_text(th, trim = TRUE) |>
        stringr::str_replace_all(" ", "_") |>
        stringr::str_to_lower()

      field_value = rvest::html_text(td, trim = TRUE)

      catalog_list[[field_name]] = field_value
    }
  }

  # Convert to one-row tibble
  if (length(catalog_list) == 0) {
    result = tibble::tibble()
  } else {
    result = tibble::as_tibble(catalog_list)
  }

  # Remove series_id and series_title columns if present
  result |>
    dplyr::select(-dplyr::any_of(c("series_id", "series_title")))
}

#' Find BLS Series by Search String
#'
#' Searches the \href{https://data.bls.gov/dataQuery/search}{BLS Data Finder}
#' for series matching a search string.
#'
#' @param search_string Character string to search for in series titles
#' @param max_results Maximum number of results to return (default: 20)
#' @param metadata Logical flag to retrieve catalog metadata (default: FALSE). When FALSE, only series_id and series_title are returned for faster performance.
#' @param survey Character string specifying a BLS survey code to restrict the search (default: NULL). When NULL, searches across all surveys. See \href{https://data.bls.gov/dataQuery/find?removeAll=1}{BLS Data Finder} for available survey codes (e.g., "cw" for CPI Urban Wage Earners, "ln" for Labor Force Statistics, "ce" for Current Employment Statistics).
#' @param seasonality Character string to filter by seasonal adjustment (default: NULL). When NULL, no filter is applied. Use "SA" to restrict to seasonally adjusted series, or "NSA" to restrict to not seasonally adjusted series.
#'
#' @returns A tibble with columns series_id, series_title, and optionally metadata (a list column containing catalog metadata as one-row tibbles when metadata = TRUE)
#'
#' @export
#' @examplesIf nzchar(Sys.getenv("BLS_API_KEY"))
#' find_bls("unemployment rate")
#'
#' find_bls("wage", survey = "cw")
find_bls = function(
  search_string,
  max_results = 20,
  metadata = FALSE,
  survey = NULL,
  seasonality = NULL
) {
  base_url = "https://data.bls.gov/dataQuery/find"

  all_results = list()
  start = 0
  rows_per_page = 20 # BLS returns 20 results per page by default

  repeat {
    # Build URL with pagination parameters
    query = list(q = search_string, st = start)

    # Build filter query (fq) parameter
    fq_filters = c()

    # Add survey filter if specified
    if (!is.null(survey)) {
      fq_filters = c(fq_filters, paste0("survey:[", survey, "]"))
    }

    # Add seasonality filter if specified
    if (!is.null(seasonality)) {
      if (seasonality == "SA") {
        fq_filters = c(fq_filters, "mcd:[Seasonally adjusted]")
      } else if (seasonality == "NSA") {
        fq_filters = c(fq_filters, "mcd:[Not seasonally adjusted]")
      }
    }

    # Build URL manually when we have multiple fq filters
    # httr::GET doesn't support multiple parameters with the same name via the query list
    if (length(fq_filters) > 1) {
      # Build the base URL with basic query parameters
      url = httr::modify_url(base_url, query = query)
      # Manually append multiple fq parameters
      fq_params = paste0(
        "fq=",
        utils::URLencode(fq_filters, reserved = TRUE),
        collapse = "&"
      )
      url = paste0(url, "&", fq_params)
      response = httr::GET(url)
    } else {
      # Add single filter to query if specified
      if (length(fq_filters) == 1) {
        query$fq = fq_filters
      }
      response = httr::GET(base_url, query = query)
    }
    httr::stop_for_status(response)

    page = httr::content(response, "parsed")

    # Extract series IDs from links
    series_links = rvest::html_nodes(
      page,
      "a[href*='dataViewer/view/timeseries/']"
    )

    if (length(series_links) == 0) {
      break # No more results
    }

    # Extract series IDs from the href attribute
    series_ids = series_links |>
      rvest::html_attr("href") |>
      stringr::str_extract("[^/]+$") # Get last part after the final slash

    # Extract series titles from data-title attributes in checkboxes
    # The data-title is in the input checkbox elements
    checkboxes = rvest::html_nodes(page, "input[type='checkbox'][data-title]")
    series_titles = checkboxes |>
      rvest::html_attr("data-title")

    # Create tibble for this page
    if (metadata) {
      # Extract catalog data for each series
      catalog_data = purrr::map(series_ids, function(series_id) {
        extract_catalog_data(page, series_id)
      })

      page_results = tibble::tibble(
        series_id = series_ids,
        series_title = series_titles,
        metadata = catalog_data
      )
    } else {
      page_results = tibble::tibble(
        series_id = series_ids,
        series_title = series_titles
      )
    }

    # Filter based on the search string if needed
    if (nrow(page_results) > 0) {
      all_results[[length(all_results) + 1]] = page_results
    }

    # Check if we've retrieved enough results or reached the end
    if (
      nrow(page_results) < rows_per_page ||
        sum(sapply(all_results, nrow)) >= max_results
    ) {
      break
    }

    # Move to next page
    start = start + rows_per_page
  }

  # Combine all results
  if (length(all_results) == 0) {
    return(empty_search_result(metadata))
  }

  final_results = dplyr::bind_rows(all_results)

  # Limit to max_results
  if (nrow(final_results) > max_results) {
    final_results = final_results[1:max_results, ]
  }

  return(final_results)
}

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
  )

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
    purrr::map(function(x) { x$footnotes <- NULL; tibble::as_tibble(x) }) |>
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
