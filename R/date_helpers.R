#' Normalize API frequency codes to standardized names
#'
#' Consolidates frequency normalization across BLS, BEA, and FRED APIs.
#'
#' @param period_code Character vector of API-specific period/frequency codes
#' @param api_type Character string specifying the API type: "bls", "bea", or "fred"
#'
#' @returns Character vector of standardized frequency names: "year", "quarter",
#'   "month", "semiyear", "week", or "day"
#' @noRd
normalize_api_frequency = function(
  period_code,
  api_type = c("bls", "bea", "fred")
) {
  api_type = match.arg(api_type)

  switch(
    api_type,
    bls = {
      # BLS uses period codes like "M01", "Q01", "A01", "S01"
      period_type = substr(period_code, 1, 1)

      dplyr::case_match(
        period_type,
        "M" ~ "month",
        "Q" ~ "quarter",
        "A" ~ "year",
        "S" ~ "semiyear"
      )
    },
    bea = {
      # BEA uses time periods like "2024Q1", "2024M01", "2024"
      dplyr::case_when(
        grepl("Q[1-4]$", period_code) ~ "quarter",
        grepl("M(0[1-9]|1[0-2])$", period_code) ~ "month",
        grepl("^[0-9]{4}$", period_code) ~ "year",
        .default = NA_character_
      )
    },
    fred = {
      # FRED uses frequency_short codes like "M", "Q", "A", "SA", "D", "W"
      dplyr::case_match(
        period_code,
        "M" ~ "month",
        "Q" ~ "quarter",
        "SA" ~ "semiyear",
        "A" ~ "year",
        "D" ~ "day",
        "W" ~ "week",
        .default = tolower(period_code)
      )
    }
  )
}


#' Parse API date from period codes
#'
#' Consolidates date parsing logic across BLS and BEA APIs.
#'
#' @param period Character vector of period/time codes
#' @param frequency Character vector of normalized frequency names (for BLS)
#' @param year Numeric vector of years (for BLS)
#'
#' @returns Date vector
#' @noRd
parse_api_date = function(period, frequency = NULL, year = NULL) {
  # Determine API type based on parameters
  if (!is.null(frequency) && !is.null(year)) {
    # BLS format: separate year and period parameters
    parse_bls_date(frequency, year, period)
  } else {
    # BEA format: combined time_period parameter
    parse_bea_date(period)
  }
}

#' @noRd
parse_bls_date = function(date_frequency, year, period) {
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
parse_bea_date = function(time_period) {
  purrr::map(time_period, function(period) {
    if (grepl("Q[1-4]$", period)) {
      # Quarterly: "2024Q1" -> 2024-01-01
      lubridate::yq(period) |> as.Date()
    } else if (grepl("M(0[1-9]|1[0-2])$", period)) {
      # Monthly: "2024M01" -> 2024-01-01
      year = substr(period, 1, 4)
      month = substr(period, 6, 7)
      lubridate::ym(paste0(year, "-", month)) |> as.Date()
    } else if (grepl("^[0-9]{4}$", period)) {
      # Annual: "2024" -> 2024-01-01
      as.Date(paste0(period, "-01-01"))
    } else {
      NA
    }
  }) |>
    unlist() |>
    as.Date(origin = "1970-01-01")
}


#' Extract period components from BEA time periods
#'
#' Consolidates extraction of year, quarter, and month from BEA time period strings.
#'
#' @param period Character vector of time periods (e.g., "2024", "2024Q1", "2024M01")
#' @param component Character string: "year", "quarter", or "month"
#'
#' @returns Integer vector of the requested component
#' @noRd
extract_period_component = function(
  period,
  component = c("year", "quarter", "month")
) {
  component = match.arg(component)

  switch(
    component,
    year = {
      purrr::map_int(period, function(p) {
        if (grepl("^[0-9]{4}", p)) {
          as.integer(substr(p, 1, 4))
        } else {
          NA_integer_
        }
      })
    },
    quarter = {
      purrr::map_int(period, function(p) {
        if (grepl("Q[1-4]$", p)) {
          as.integer(substr(p, nchar(p), nchar(p)))
        } else {
          NA_integer_
        }
      })
    },
    month = {
      purrr::map_int(period, function(p) {
        if (grepl("M(0[1-9]|1[0-2])$", p)) {
          as.integer(substr(p, 6, 7))
        } else {
          NA_integer_
        }
      })
    }
  )
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
