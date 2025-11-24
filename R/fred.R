#' Find FRED Series by Search String
#'
#' Searches the \href{https://fred.stlouisfed.org/}{FRED database}
#' for series matching a search string.
#'
#' This function is a wrapper around \href{https://sboysel.github.io/fredr/}{`fredr::fredr_series_search_text()`}
#' and requires you to have an \href{https://fred.stlouisfed.org/docs/api/api_key.html}{FRED API key}
#' saved in the `FRED_API_KEY` environment variable.
#'
#' @param search_string Character string to search for in series titles
#' @param max_results Maximum number of results to return (default: 20)
#' @param metadata Logical flag to retrieve additional metadata (default: FALSE). When FALSE, only series_id and series_title are returned.
#' @param seasonality Optional filter for seasonal adjustment: NULL (no filter, default), "NSA" (Not Seasonally Adjusted), or "SA" (Seasonally Adjusted)
#'
#' @returns A tibble with columns series_id, series_title, and optionally metadata (a list column containing additional metadata when metadata = TRUE)
#'
#' @export
#' @examplesIf FALSE
#' find_fred("unemployment rate")
#' find_fred("GDP", max_results = 50)
#' find_fred("inflation", metadata = TRUE)
#' find_fred("unemployment", seasonality = "SA")
#' find_fred("employment", seasonality = "NSA")
find_fred = function(
  search_string,
  max_results = 20,
  metadata = FALSE,
  seasonality = NULL
) {
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

#' Retrieve data from the FRED API
#'
#' This function is simply a wrapper around \href{https://sboysel.github.io/fredr/}{`fredr`} and requires you to have an \href{https://fred.stlouisfed.org/docs/api/api_key.html}{FRED API key} saved in the `FRED_API_KEY` environment variable.

#' @param series FRED series code
#' @param start Start year or date (numeric year or Date object)
#' @param end End year or date (numeric year or Date object)
#' @param metadata Flag for additional metadata
#'
#' @returns A tibble
#' @export
#' @examplesIf FALSE
#' get_fred("UNRATE")
#'
#' series = c(
#'   gdp = "GDP",
#'   urate = "UNRATE"
#' )
#'
#' get_fred(series, start = as.Date("2024-07-01"), end = 2024)
#'
#' # Tibble with list columns including additional metadata
#' complete_results = get_fred(series, start = 2020, end = 2025, metadata = T)
#' complete_results
#'
#' complete_results |>
#'   tidyr::unnest(metadata)
get_fred = function(series, start = NULL, end = NULL, metadata = F) {
  # Convert start/end to Date objects if provided
  observation_start = convert_to_date(start, is_start = TRUE)
  observation_end = convert_to_date(end, is_start = FALSE)

  # Fetch complete results with metadata and data as list columns
  complete_results = fetch_fred_complete(
    series,
    observation_start,
    observation_end
  )

  if (metadata) {
    complete_results
  } else {
    extract_data(complete_results, fred_series_data_extractor)
  }
}

convert_to_date = function(date_input, is_start) {
  if (is.null(date_input)) {
    return(NULL)
  }

  # If already a Date object, return as-is
  if (inherits(date_input, "Date")) {
    return(date_input)
  }

  # If numeric, treat as year
  if (is.numeric(date_input)) {
    if (is_start) {
      # Start of year: January 1
      return(as.Date(paste0(date_input, "-01-01")))
    } else {
      # End of year: December 31
      return(as.Date(paste0(date_input, "-12-31")))
    }
  }

  # Invalid input type
  stop("start and end must be numeric years or Date objects")
}

#' @noRd
normalize_fred_frequency = function(frequency_short) {
  # Map FRED frequency codes to standardized names
  dplyr::case_match(
    frequency_short,
    "M" ~ "month",
    "Q" ~ "quarter",
    "SA" ~ "semiyear",
    "A" ~ "year",
    "D" ~ "day",
    "W" ~ "week",
    .default = tolower(frequency_short)
  )
}

#' @noRd
fetch_fred_complete = function(series, observation_start, observation_end) {
  # Fetch data for all series, creating list columns for metadata and data
  complete_results = seq_along(series) |>
    purrr::map(
      ~ fetch_fred_series_complete(
        .x,
        series,
        observation_start,
        observation_end
      )
    ) |>
    purrr::list_rbind()

  add_series_names(complete_results, series)
}

#' @noRd
fetch_fred_series_complete = function(
  idx,
  series,
  observation_start,
  observation_end
) {
  # Get the series ID by index
  series_id = series[[idx]]

  # Fetch series metadata
  metadata = fredr::fredr_series(series_id = series_id)

  # Fetch data from FRED API
  raw_data = fredr::fredr(
    series_id = series_id,
    observation_start = observation_start,
    observation_end = observation_end
  )

  # Process data with date components
  data = raw_data |>
    dplyr::select(
      -dplyr::all_of(c("series_id", "realtime_start", "realtime_end"))
    ) |>
    dplyr::mutate(value = as.numeric(.data$value))

  # Return as tibble with list columns
  tibble::tibble(
    series_id = series_id,
    metadata = list(metadata),
    data = list(data)
  )
}

#' @noRd
fred_series_data_extractor = function(series_id, complete_results) {
  complete_results |>
    dplyr::filter(.data$series_id == {{ series_id }}) |>
    tidyr::unnest("metadata") |>
    dplyr::mutate(
      date_frequency = normalize_fred_frequency(.data$frequency_short)
    ) |>
    dplyr::rename(series_title = .data$title) |>
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
  get_fred("UNRATE")

  series = c(
    gdp = "GDP",
    urate = "UNRATE"
  )

  get_fred(series, start = as.Date("2024-07-01"), end = 2024)

  complete_results = get_fred(series, start = 2020, end = 2025, metadata = T)

  complete_results

  complete_results |>
    tidyr::unnest(metadata)
}
