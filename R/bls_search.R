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
