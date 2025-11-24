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
#' @examplesIf FALSE
#' find_bls("unemployment rate")
#' find_bls("employment population ratio", max_results = 50)
#' find_bls("unemployment rate", metadata = TRUE)
#' find_bls("wage", survey = "cw")
#' find_bls("unemployment", seasonality = "SA")
#' find_bls("unemployment", seasonality = "NSA")
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
      fq_params = paste0("fq=", utils::URLencode(fq_filters, reserved = TRUE), collapse = "&")
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
    if (metadata) {
      return(tibble::tibble(
        series_id = character(),
        series_title = character(),
        metadata = list()
      ))
    } else {
      return(tibble::tibble(
        series_id = character(),
        series_title = character()
      ))
    }
  }

  final_results = dplyr::bind_rows(all_results)

  # Limit to max_results
  if (nrow(final_results) > max_results) {
    final_results = final_results[1:max_results, ]
  }

  return(final_results)
}

#' Retrieve data from the BLS API
#'
#' This function is simply a wrapper around \href{https://github.com/groditi/blsR}{`blsR`} and requires you to have an \href{https://www.bls.gov/developers/}{BLS API key} saved in the `BLS_API_KEY` environment variable.
#'
#' @param series BLS series code
#' @param start Start year (numeric)
#' @param end End year (numeric)
#' @param metadata Flag for additional metadata
#'
#' @returns a tibble
#'
#'
#' @export
#' @examplesIf FALSE
#' get_bls("LNU02300060", start = 2020, end = 2024)
#'
#' bls_series_ids = c(
#'   "LNU02300060",
#'   emp_fb_2534 = "LNU02073399",
#'   epop_asianmen_2554 = "LNU02332330Q",
#'   injuries_annual = "ISU00000000031004",
#'   cpi_semi = "CUUS0000SA0"
#' )
#'
#' get_bls(bls_series_ids, start = 2025, end = 2025)
#'
#' # Tibble with list columns including additional metadata
#' complete_results = get_bls(bls_series_ids, start = 2025, end = 2025, metadata = T)
#' complete_results
#'
#' complete_results |>
#'   tidyr::unnest(metadata)
get_bls = function(series, start, end, metadata = F) {
  raw_results = blsR::get_n_series(
    series,
    start_year = start,
    end_year = end,
    catalog = T
  )

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
  metadata = payload |>
    purrr::pluck(id, "catalog") |>
    tibble::as_tibble() |>
    dplyr::select(-c("series_id"))

  data = payload |>
    purrr::pluck(id, "data") |>
    purrr::map(tibble::as_tibble) |>
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
      date_frequency = normalize_bls_frequency(.data$period),
      date = date_from_bls_frequency(
        .data$date_frequency,
        .data$year,
        .data$period
      )
    )
}

#' @noRd
normalize_bls_frequency = function(period) {
  period_type = substr(period, 1, 1)

  dplyr::case_match(
    period_type,
    "M" ~ "month",
    "Q" ~ "quarter",
    "A" ~ "year",
    "S" ~ "semiyear"
  )
}

#' @noRd
date_from_bls_frequency = function(date_frequency, year, period) {
  period_num = as.numeric(substr(period, 2, 3))

  if (date_frequency == "semiyear") {
    period_num = dplyr::case_match(period_num, 1 ~ 1, 2 ~ 7)
  }

  year_period = paste(year, period_num)

  ifelse(
    date_frequency == "quarter",
    lubridate::yq(year_period),
    lubridate::ym(year_period)
  ) |>
    as.Date()
}

#' @noRd
bls_series_data_extractor = function(series_id, complete_results) {
  complete_results |>
    dplyr::filter(.data$series_id == {{ series_id }}) |>
    tidyr::unnest("metadata") |>
    dplyr::select(dplyr::any_of(c(
      "name",
      "series_id",
      "series_title",
      "data"
    ))) |>
    tidyr::unnest("data") |>
    dplyr::select(dplyr::any_of(c(
      "name",
      "series_id",
      "series_title",
      "date_frequency",
      "date",
      "value"
    )))
}

# scratchpad
function() {
  get_bls("LNU02300060", start = 2020, end = 2024)

  bls_series_ids = c(
    "LNU02300060",
    emp_fb_2534 = "LNU02073399",
    epop_asianmen_2554 = "LNU02332330Q",
    injuries_annual = "ISU00000000031004",
    cpi_semi = "CUUS0000SA0"
  )

  get_bls(bls_series_ids, start = 2025, end = 2025)

  get_bls(bls_series_ids, start = 2025, end = 2025, metadata = T)
}
