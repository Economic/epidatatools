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
