#' Find FRED Series by Search String
#'
#' Searches the \href{https://fred.stlouisfed.org/}{FRED database}
#' for series matching a search string.
#'
#' This function is a wrapper around \href{https://sboysel.github.io/fredr/}{`fredr::fredr_series_search_text()`}
#' and requires you to have an \href{https://fred.stlouisfed.org/docs/api/api_key.html}{FRED API key}.
#'
#' @param search_string Character string to search for in series titles
#' @param max_results Maximum number of results to return (default: 20)
#' @param metadata Logical flag to retrieve additional metadata (default: FALSE). When FALSE, only series_id and series_title are returned.
#' @param seasonality Optional filter for seasonal adjustment: NULL (no filter, default), "NSA" (Not Seasonally Adjusted), or "SA" (Seasonally Adjusted)
#' @param fred_api_key FRED API key (defaults to FRED_API_KEY environment variable)
#'
#' @returns A tibble with columns series_id, series_title, and optionally metadata (a list column containing additional metadata when metadata = TRUE)
#'
#' @export
#' @examplesIf FALSE
#' find_fred("unemployment rate")
#'
#' find_fred("GDP", max_results = 10)
#'
#' find_fred("inflation", metadata = TRUE)
#'
#' find_fred("employment population ratio", seasonality = "SA")
find_fred = function(
  search_string,
  max_results = 20,
  metadata = FALSE,
  seasonality = NULL,
  fred_api_key = Sys.getenv("FRED_API_KEY")
) {
  validate_api_key(
    fred_api_key,
    "FRED",
    "https://fred.stlouisfed.org/docs/api/fred/v2/api_key.html"
  )

  # Determine filter parameters based on seasonality
  if (!is.null(seasonality)) {
    # Validate seasonality argument
    if (!seasonality %in% c("NSA", "SA")) {
      stop("seasonality must be NULL, 'NSA', or 'SA'")
    }

    filter_variable = "seasonal_adjustment"
    filter_value = if (seasonality == "NSA") {
      "Not Seasonally Adjusted"
    } else {
      "Seasonally Adjusted"
    }

    # Set API key once before using fredr
    fredr::fredr_set_key(fred_api_key)

    # Call fredr_series_search_text with filtering
    search_results = fredr::fredr_series_search_text(
      search_text = search_string,
      limit = max_results,
      filter_variable = filter_variable,
      filter_value = filter_value
    )
  } else {
    # Call fredr_series_search_text without filtering
    search_results = fredr::fredr_series_search_text(
      search_text = search_string,
      limit = max_results
    )
  }

  # If no results, return empty tibble
  if (nrow(search_results) == 0) {
    return(empty_search_result(metadata))
  }

  # Rename id to series_id and title to series_title
  if (metadata) {
    # Create a list column with all other columns as metadata
    search_results |>
      dplyr::rename(series_id = .data$id, series_title = .data$title) |>
      tidyr::nest(metadata = -c(.data$series_id, .data$series_title)) |>
      dplyr::select(.data$series_id, .data$series_title, .data$metadata)
  } else {
    # Return only series_id and series_title
    search_results |>
      dplyr::select(series_id = .data$id, series_title = .data$title)
  }
}
