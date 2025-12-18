#' Retrieve data from the FRED API
#'
#' This function is simply a wrapper around \href{https://sboysel.github.io/fredr/}{`fredr`} and requires you to have an \href{https://fred.stlouisfed.org/docs/api/api_key.html}{FRED API key}.

#' @param series FRED series code
#' @param start Start year or date (numeric year or Date object)
#' @param end End year or date (numeric year or Date object)
#' @param metadata Flag for additional metadata
#' @param fred_api_key FRED API key (defaults to FRED_API_KEY environment variable)
#'
#' @returns A tibble
#' @export
#' @examplesIf nzchar(Sys.getenv("FRED_API_KEY"))
#' get_fred("UNRATE")
#'
#' series = c(
#'   gdp = "GDP",
#'   urate = "UNRATE"
#' )
#' get_fred(series, start = as.Date("2024-07-01"), end = 2024)
#'
#' complete_results = get_fred(series, start = 2020, end = 2025, metadata = TRUE)
#' complete_results
#'
#' complete_results |>
#'   tidyr::unnest(metadata)
get_fred = function(
  series,
  start = NULL,
  end = NULL,
  metadata = FALSE,
  fred_api_key = Sys.getenv("FRED_API_KEY")
) {
  validate_api_key(
    fred_api_key,
    "FRED",
    "https://fred.stlouisfed.org/docs/api/fred/v2/api_key.html"
  )

  # Convert start/end to Date objects if provided
  observation_start = convert_to_date(start, is_start = TRUE)
  observation_end = convert_to_date(end, is_start = FALSE)

  # Fetch complete results with metadata and data as list columns
  complete_results = fetch_fred_complete(
    series,
    observation_start,
    observation_end,
    fred_api_key
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
fetch_fred_complete = function(
  series,
  observation_start,
  observation_end,
  fred_api_key
) {
  # Set API key once before fetching all series
  fredr::fredr_set_key(fred_api_key)

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
  # Define transformation to add date_frequency and rename title
  fred_transform = function(data) {
    data |>
      dplyr::mutate(
        date_frequency = normalize_api_frequency(.data$frequency_short, "fred")
      ) |>
      dplyr::rename(series_title = .data$title)
  }

  generic_data_extractor(
    series_id,
    complete_results,
    metadata_cols = c("name", "series_id", "title", "frequency_short", "data"),
    final_cols = c(
      "name",
      "series_id",
      "series_title",
      "date_frequency",
      "date",
      "value"
    ),
    transform_fn = fred_transform
  )
}
