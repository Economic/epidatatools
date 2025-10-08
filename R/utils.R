#' @noRd
dummy <- function() {
  utils::globalVariables
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
semiyear = function(date) {
  ifelse(lubridate::month(date) < 7, 1, 2)
}

#' @noRd
add_all_date_components = function(data) {
  data_with_year = data |>
    dplyr::mutate(year = lubridate::year(date))

  data_with_year |>
    dplyr::mutate(year = lubridate::year(date)) |>
    add_date_component("month") |>
    add_date_component("quarter") |>
    add_date_component("semiyear") |>
    add_date_component("week") |>
    add_date_component("day")
}

#' @noRd
add_date_component = function(data, var) {
  frequencies = data |>
    dplyr::distinct(.data$date_frequency) |>
    dplyr::pull()

  will_add_var = var %in% frequencies

  if (will_add_var) {
    output = switch(
      var,
      "day" = dplyr::mutate(data, day = lubridate::day(date)),
      "week" = dplyr::mutate(data, week = lubridate::week(date)),
      "month" = dplyr::mutate(data, month = lubridate::month(date)),
      "quarter" = dplyr::mutate(data, quarter = lubridate::quarter(date)),
      "semiyear" = dplyr::mutate(data, semiyear = semiyear(date))
    )
  } else {
    output = data
  }

  output
}

#' @noRd
extract_data = function(data, extractor) {
  data |>
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
}
