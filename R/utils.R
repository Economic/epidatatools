#' @noRd
dummy <- function() {
  utils::globalVariables
}

#' Return empty search result tibble
#'
#' Consolidates empty result creation for find_* functions
#'
#' @param metadata Logical flag to include metadata column
#' @returns Empty tibble with correct schema
#' @noRd
empty_search_result = function(metadata = FALSE) {
  if (metadata) {
    tibble::tibble(
      series_id = character(),
      series_title = character(),
      metadata = list()
    )
  } else {
    tibble::tibble(
      series_id = character(),
      series_title = character()
    )
  }
}

#' Validate API key and throw error if not set
#'
#' Consolidates API key validation across API functions
#'
#' @param api_key Character string of the API key
#' @param api_name Name of the API (e.g., "BLS", "BEA", "FRED")
#' @param signup_url URL to sign up for API key
#' @noRd
validate_api_key = function(api_key, api_name, signup_url) {
  if (api_key == "") {
    stop(
      api_name,
      "_API_KEY environment variable not set. Get a free API key at ",
      signup_url
    )
  }
}

#' @noRd
add_series_names = function(data, series) {
  series_names = names(series)

  if (is.null(series_names)) {
    data
  } else {
    data |>
      dplyr::mutate(
        name = series_names[series == .data$series_id],
        .before = "series_id"
      ) |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "", NA, .data$name)
      )
  }
}


#' @noRd
extract_data = function(data, extractor) {
  final_data = data |>
    dplyr::pull(.data$series_id) |>
    purrr::map(~ extractor(.x, data)) |>
    purrr::list_rbind() |>
    add_all_date_components() |>
    dplyr::select(dplyr::any_of(c(
      "name",
      "series_id",
      "series_title",
      "date_frequency",
      "date",
      "year",
      "semiyear",
      "quarter",
      "month",
      "week",
      "day",
      "value"
    )))

  # would be better to have a check here about numeric values
  # and not convert if it would introduce missings
  final_data |>
    dplyr::mutate(value = as.numeric(.data$value))
}

#' Generic data extractor for API results
#'
#' Consolidates the common pattern across BLS and FRED API data extractors.
#' Filters by series_id, unnests metadata and data, and selects final columns.
#'
#' @param series_id Character string of the series ID to extract
#' @param complete_results Tibble with series_id, metadata (list), and data (list) columns
#' @param metadata_cols Character vector of columns to select after unnesting metadata
#' @param final_cols Character vector of final columns to select after unnesting data
#' @param transform_fn Optional function to apply transformations between unnesting metadata and data
#'
#' @returns Tibble with extracted and processed data
#' @noRd
generic_data_extractor = function(
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
  ),
  transform_fn = NULL
) {
  result = complete_results |>
    dplyr::filter(.data$series_id == .env$series_id) |>
    tidyr::unnest("metadata") |>
    dplyr::select(dplyr::any_of(metadata_cols))

  # Apply transformation if provided
  if (!is.null(transform_fn)) {
    result = transform_fn(result)
  }

  result |>
    tidyr::unnest("data") |>
    dplyr::select(dplyr::any_of(final_cols))
}
