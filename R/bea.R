#' Make a generic BEA API call
#'
#' @param method API method to call. One of: "GetDatasetList", "GetParameterList",
#'   "GetParameterValues", "GetParameterValuesFiltered", or "GetData" (default)
#' @param dataset_name Name of the BEA dataset (e.g., "NIPA", "Regional", "GDPbyIndustry",
#'   "UnderlyingGDPbyIndustry"). Not required for GetDatasetList method.
#' @param params Named list of parameters for the API call
#' @param bea_api_key BEA API key (defaults to BEA_API_KEY environment variable)
#'
#' @returns Parsed JSON response from the BEA API
#' @noRd
get_bea_api = function(
  method = "GetData",
  dataset_name = NULL,
  params = list(),
  bea_api_key = Sys.getenv("BEA_API_KEY")
) {
  # Check for API key
  if (bea_api_key == "") {
    stop(
      "BEA_API_KEY environment variable not set. Get a free API key at https://www.bea.gov/API/signup/index.cfm"
    )
  }

  # Build query parameters
  query = list(
    UserID = bea_api_key,
    method = method,
    ResultFormat = "JSON"
  )

  # Add DatasetName if provided
  if (!is.null(dataset_name)) {
    query$DatasetName = dataset_name
  }

  # Add additional parameters
  query = c(query, params)

  # Make API request
  response = httr::GET(
    "https://apps.bea.gov/api/data/",
    query = query
  )

  httr::stop_for_status(response)

  # Parse JSON response
  content = httr::content(response, as = "text", encoding = "UTF-8")
  result = jsonlite::fromJSON(content, simplifyVector = FALSE)

  # Check for API errors
  if (!is.null(result$BEAAPI$Error)) {
    error_msg = result$BEAAPI$Error$APIErrorDescription
    stop("BEA API error: ", error_msg)
  }

  return(result)
}

#' Retrieve NIPA data from the BEA API
#'
#' Retrieves National Income and Product Accounts data from the
#' \href{https://apps.bea.gov/API/signup/}{Bureau of Economic Analysis API}.
#' Requires a BEA API key saved in the `BEA_API_KEY` environment variable.
#'
#' @param tables Character vector of NIPA table names (e.g., "T10101", "T20305"). Can be a named vector to add custom names.
#' @param years Numeric vector of years or "ALL" for all available years
#' @param frequency Character string: "year" (annual), "quarter" (quarterly), or "month" (monthly). Can specify multiple as comma-separated string.
#' @param underlying Logical flag to use NIUnderlyingDetail dataset instead of NIPA (default: FALSE)
#' @param metadata Logical flag to return additional metadata columns (default: FALSE)
#' @param bea_api_key BEA API key (defaults to BEA_API_KEY environment variable)
#'
#' @returns A tibble with columns: table_name, table_description, line_number, line_description, date_frequency, date, value, and date variables (year, quarter, month) appropriate to the frequency of data returned. Annual data includes only year; quarterly data includes year and quarter; monthly data includes year, quarter, and month. If metadata = TRUE, also includes unit_mult, metric_name, cl_unit, series_code, and note_text (list column). If tables is a named vector, includes a "name" column as the first column.
#'
#' @export
#' @examplesIf FALSE
#' # Get quarterly GDP data
#' get_bea_nipa("T10101", years = 2020:2024, frequency = "quarter")
#'
#' # Get annual data for multiple tables
#' get_bea_nipa(c("T10101", "T20305"), years = "ALL", frequency = "year")
#'
#' # Use named vector to add custom names
#' get_bea_nipa(
#'   c("gdp" = "T10101", "personal_income" = "T20305"),
#'   years = 2023:2024,
#'   frequency = "year"
#' )
#'
#' # Get data with additional metadata
#' get_bea_nipa("T10101", years = 2023, frequency = "year", metadata = TRUE)
get_bea_nipa = function(
  tables,
  years,
  frequency = c("year", "quarter", "month"),
  underlying = FALSE,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
) {
  # Normalize frequency parameter
  frequency = match.arg(frequency, several.ok = TRUE)
  freq_code = dplyr::case_match(
    frequency,
    "year" ~ "A",
    "quarter" ~ "Q",
    "month" ~ "M"
  )
  freq_param = paste(freq_code, collapse = ",")

  # Normalize years parameter
  if (is.character(years) && toupper(years) == "ALL") {
    years_param = "ALL"
  } else {
    years_param = paste(years, collapse = ",")
  }

  # Choose dataset
  dataset_name = if (underlying) "NIUnderlyingDetail" else "NIPA"

  # Fetch data for all tables
  complete_results = seq_along(tables) |>
    purrr::map(
      ~ fetch_bea_nipa_complete(
        .x,
        tables,
        years_param,
        freq_param,
        dataset_name,
        bea_api_key
      )
    ) |>
    purrr::list_rbind()

  # Add series names if tables are named
  table_names = names(tables)
  if (!is.null(table_names)) {
    complete_results = complete_results |>
      dplyr::mutate(
        name = table_names[match(.data$table_name, tables)],
        .before = 1
      ) |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "", NA_character_, .data$name)
      )
  }

  # Determine which date columns to include based on frequencies present in data
  unique_frequencies = unique(complete_results$date_frequency)

  # Start with always-included date columns
  date_cols = c("date", "year")

  # Add quarter column if any quarterly or monthly data present
  if (any(c("quarter", "month") %in% unique_frequencies)) {
    date_cols = c(date_cols, "quarter")
  }

  # Add month column only if monthly data present
  if ("month" %in% unique_frequencies) {
    date_cols = c(date_cols, "month")
  }

  # Select columns based on metadata flag
  if (metadata) {
    complete_results
  } else {
    complete_results |>
      dplyr::select(
        dplyr::any_of(c(
          "name",
          "table_name",
          "table_description",
          "line_number",
          "line_description",
          "date_frequency",
          date_cols,
          "value"
        ))
      )
  }
}

#' @noRd
fetch_bea_nipa_complete = function(
  idx,
  tables,
  years_param,
  freq_param,
  dataset_name,
  bea_api_key
) {
  table_name = tables[[idx]]

  # Make API call
  result = get_bea_api(
    dataset_name = dataset_name,
    params = list(
      TableName = table_name,
      Frequency = freq_param,
      Year = years_param
    ),
    bea_api_key = bea_api_key
  )

  # Extract data and notes from response
  raw_data = result$BEAAPI$Results$Data
  raw_notes = result$BEAAPI$Results$Notes

  # Extract table description (first note text where NoteRef matches table name)
  table_description = NA_character_
  if (!is.null(raw_notes) && length(raw_notes) > 0) {
    # Find the note that matches the table name
    for (note in raw_notes) {
      if (!is.null(note$NoteRef) && note$NoteRef == table_name) {
        table_description = note$NoteText %||% NA_character_
        break
      }
    }
  }

  # Collect all note texts for the note_text list column
  all_note_texts = purrr::map_chr(raw_notes, ~ .x$NoteText %||% NA_character_)

  # Convert to tibble
  data_tibble = purrr::map_dfr(raw_data, function(row) {
    tibble::tibble(
      table_name = row$TableName %||% NA_character_,
      table_description = table_description,
      series_code = row$SeriesCode %||% NA_character_,
      line_number = as.integer(row$LineNumber %||% NA),
      line_description = row$LineDescription %||% NA_character_,
      time_period = row$TimePeriod %||% NA_character_,
      metric_name = row$METRIC_NAME %||% NA_character_,
      cl_unit = row$CL_UNIT %||% NA_character_,
      unit_mult = as.integer(row$UNIT_MULT %||% NA),
      data_value = row$DataValue %||% NA_character_,
      note_ref = row$NoteRef %||% NA_character_
    )
  })

  # Process dates, frequencies, and add year/month/quarter columns
  data_processed = data_tibble |>
    dplyr::mutate(
      date_frequency = normalize_bea_frequency(.data$time_period),
      date = date_from_bea_period(.data$time_period),
      year = extract_year_from_period(.data$time_period),
      quarter = extract_quarter_from_period(.data$time_period),
      month = extract_month_from_period(.data$time_period),
      value = as.numeric(gsub(",", "", .data$data_value)),
      note_text = list(all_note_texts)
    )

  # Return flat tibble
  data_processed |>
    dplyr::select(
      .data$table_name,
      .data$table_description,
      .data$line_number,
      .data$line_description,
      .data$date_frequency,
      .data$date,
      .data$year,
      .data$quarter,
      .data$month,
      .data$value,
      .data$unit_mult,
      .data$metric_name,
      .data$cl_unit,
      .data$series_code,
      .data$note_text
    )
}

#' @noRd
normalize_bea_frequency = function(time_period) {
  # Extract frequency from time period (e.g., "2024Q1" -> "quarter", "2024" -> "year")
  dplyr::case_when(
    grepl("Q[1-4]$", time_period) ~ "quarter",
    grepl("M(0[1-9]|1[0-2])$", time_period) ~ "month",
    grepl("^[0-9]{4}$", time_period) ~ "year",
    .default = NA_character_
  )
}

#' @noRd
date_from_bea_period = function(time_period) {
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

#' @noRd
extract_year_from_period = function(time_period) {
  purrr::map_int(time_period, function(period) {
    if (grepl("^[0-9]{4}", period)) {
      as.integer(substr(period, 1, 4))
    } else {
      NA_integer_
    }
  })
}

#' @noRd
extract_quarter_from_period = function(time_period) {
  purrr::map_int(time_period, function(period) {
    if (grepl("Q[1-4]$", period)) {
      as.integer(substr(period, nchar(period), nchar(period)))
    } else {
      NA_integer_
    }
  })
}

#' @noRd
extract_month_from_period = function(time_period) {
  purrr::map_int(time_period, function(period) {
    if (grepl("M(0[1-9]|1[0-2])$", period)) {
      as.integer(substr(period, 6, 7))
    } else {
      NA_integer_
    }
  })
}


#' Retrieve Regional data from the BEA API
#'
#' Retrieves regional economic data from the
#' \href{https://apps.bea.gov/API/signup/}{Bureau of Economic Analysis API}.
#' Requires a BEA API key saved in the `BEA_API_KEY` environment variable.
#'
#' @param geo_fips Character vector of geographic FIPS codes, or special values: "COUNTY" for all counties, "STATE" for all states, "MSA" for all MSAs, or state abbreviations (e.g., "NY", "CA")
#' @param tables Character vector of table names (e.g., "CAINC1" for personal income)
#' @param years Numeric vector of years or "ALL" for all available years
#' @param line_code Integer or character vector of line codes specifying the statistics/industries to retrieve
#' @param metadata Logical flag to return additional metadata (default: FALSE)
#' @param bea_api_key BEA API key (defaults to BEA_API_KEY environment variable)
#'
#' @returns A tibble with columns series_id (constructed from geo/table/line), series_title, date_frequency, date, and value. If metadata = TRUE, returns a tibble with list columns for metadata and data.
#'
# #' @export
#' @noRd
#' @examplesIf FALSE
#' # Get personal income for all states
#' get_bea_regional("STATE", tables = "SAINC1", years = 2020:2024, line_code = 1)
#'
#' # Get data for specific states
#' get_bea_regional(c("NY", "CA"), tables = "SAINC1", years = "ALL", line_code = 1)
#'
#' # Named series
#' states = c(new_york = "NY", california = "CA")
#' get_bea_regional(states, tables = "SAINC1", years = 2020:2024, line_code = 1)
get_bea_regional = function(
  geo_fips,
  tables,
  years,
  line_code,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
) {
  # Normalize parameters
  geo_param = paste(geo_fips, collapse = ",")
  table_param = paste(tables, collapse = ",")

  if (is.character(years) && toupper(years) == "ALL") {
    years_param = "ALL"
  } else {
    years_param = paste(years, collapse = ",")
  }

  line_param = paste(line_code, collapse = ",")

  # Make API call
  result = get_bea_api(
    dataset_name = "Regional",
    params = list(
      GeoFips = geo_param,
      TableName = table_param,
      LineCode = line_param,
      Year = years_param
    ),
    bea_api_key = bea_api_key
  )

  # Extract and process data
  raw_data = result$BEAAPI$Results$Data

  # Convert to tibble
  data_tibble = purrr::map_dfr(raw_data, function(row) {
    tibble::tibble(
      geo_fips = row$GeoFips %||% NA_character_,
      geo_name = row$GeoName %||% NA_character_,
      table_name = row$Code %||% NA_character_,
      line_code = row$LineCode %||% NA_character_,
      description = row$Description %||% NA_character_,
      time_period = row$TimePeriod %||% NA_character_,
      cl_unit = row$CL_UNIT %||% NA_character_,
      unit_mult = as.integer(row$UNIT_MULT %||% NA),
      data_value = row$DataValue %||% NA_character_
    )
  })

  # Group by unique series (geo_fips + table + line_code)
  series_groups = data_tibble |>
    dplyr::group_by(
      .data$geo_fips,
      .data$geo_name,
      .data$table_name,
      .data$line_code,
      .data$description
    ) |>
    dplyr::group_split()

  # Process each series
  complete_results = purrr::map_dfr(series_groups, function(group) {
    series_id = paste0(
      unique(group$geo_fips),
      "_",
      unique(group$table_name),
      "_",
      unique(group$line_code)
    )

    # Create metadata
    metadata_tibble = group |>
      dplyr::select(
        .data$geo_fips,
        .data$geo_name,
        .data$table_name,
        .data$line_code,
        .data$description,
        .data$cl_unit,
        .data$unit_mult
      ) |>
      dplyr::distinct()

    # Create data tibble
    data_only = group |>
      dplyr::mutate(
        date_frequency = "year", # Regional data is typically annual
        date = as.Date(paste0(.data$time_period, "-01-01")),
        value = .data$data_value
      ) |>
      dplyr::select(.data$date_frequency, .data$date, .data$value)

    tibble::tibble(
      series_id = series_id,
      series_title = paste(
        unique(group$geo_name),
        unique(group$description),
        sep = " - "
      ),
      metadata = list(metadata_tibble),
      data = list(data_only)
    )
  })

  # Add series names if geo_fips are named
  complete_results = add_series_names(complete_results, geo_fips)

  if (metadata) {
    complete_results
  } else {
    extract_data(complete_results, bea_regional_data_extractor)
  }
}

#' @noRd
bea_regional_data_extractor = function(series_id, complete_results) {
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

#' Retrieve Industry GDP data from the BEA API
#'
#' Retrieves GDP by Industry data from the
#' \href{https://apps.bea.gov/API/signup/}{Bureau of Economic Analysis API}.
#' Requires a BEA API key saved in the `BEA_API_KEY` environment variable.
#'
#' @param tables Character vector of table IDs (e.g., "1" for value added, "2" for compensation)
#' @param years Numeric vector of years or "ALL" for all available years
#' @param frequency Character string: "year" (annual) or "quarter" (quarterly)
#' @param industry Character vector of industry codes (e.g., "11" for agriculture, "ALL" for all industries)
#' @param underlying Logical flag to use UnderlyingGDPbyIndustry dataset instead of GDPbyIndustry (default: FALSE)
#' @param metadata Logical flag to return additional metadata (default: FALSE)
#' @param bea_api_key BEA API key (defaults to BEA_API_KEY environment variable)
#'
#' @returns A tibble with columns series_id (constructed from table/industry), series_title, date_frequency, date, and value. If metadata = TRUE, returns a tibble with list columns for metadata and data.
#'
# #' @export
#' @noRd
#' @examplesIf FALSE
#' # Get quarterly value added for all industries
#' get_bea_industry("1", years = 2020:2024, frequency = "quarter", industry = "ALL")
#'
#' # Get annual data for specific industries
#' get_bea_industry("1", years = 2020:2024, frequency = "year", industry = c("11", "21"))
#'
#' # Named series
#' industries = c(agriculture = "11", mining = "21")
#' get_bea_industry("1", years = 2020:2024, frequency = "year", industry = industries)
get_bea_industry = function(
  tables,
  years,
  frequency = c("year", "quarter"),
  industry,
  underlying = FALSE,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
) {
  # Normalize frequency parameter
  frequency = match.arg(frequency, several.ok = TRUE)
  freq_code = dplyr::case_match(
    frequency,
    "year" ~ "A",
    "quarter" ~ "Q"
  )
  freq_param = paste(freq_code, collapse = ",")

  # Normalize years parameter
  if (is.character(years) && toupper(years) == "ALL") {
    years_param = "ALL"
  } else {
    years_param = paste(years, collapse = ",")
  }

  # Normalize industry parameter
  industry_param = paste(industry, collapse = ",")

  # Normalize table parameter
  table_param = paste(tables, collapse = ",")

  # Choose dataset
  dataset_name = if (underlying) "UnderlyingGDPbyIndustry" else "GDPbyIndustry"

  # Make API call
  result = get_bea_api(
    dataset_name = dataset_name,
    params = list(
      TableID = table_param,
      Frequency = freq_param,
      Year = years_param,
      Industry = industry_param
    ),
    bea_api_key = bea_api_key
  )

  # Extract and process data
  raw_data = result$BEAAPI$Results$Data

  # Convert to tibble
  data_tibble = purrr::map_dfr(raw_data, function(row) {
    tibble::tibble(
      table_id = row$TableID %||% NA_character_,
      industry = row$Industry %||% NA_character_,
      industry_description = row$IndustryDescription %||% NA_character_,
      time_period = row$TimePeriod %||% NA_character_,
      cl_unit = row$CL_UNIT %||% NA_character_,
      unit_mult = as.integer(row$UNIT_MULT %||% NA),
      data_value = row$DataValue %||% NA_character_
    )
  })

  # Group by unique series (table + industry)
  series_groups = data_tibble |>
    dplyr::group_by(
      .data$table_id,
      .data$industry,
      .data$industry_description
    ) |>
    dplyr::group_split()

  # Process each series
  complete_results = purrr::map_dfr(series_groups, function(group) {
    series_id = paste0(
      unique(group$table_id),
      "_",
      unique(group$industry)
    )

    # Create metadata
    metadata_tibble = group |>
      dplyr::select(
        .data$table_id,
        .data$industry,
        .data$industry_description,
        .data$cl_unit,
        .data$unit_mult
      ) |>
      dplyr::distinct()

    # Create data tibble
    data_only = group |>
      dplyr::mutate(
        date_frequency = normalize_bea_frequency(.data$time_period),
        date = date_from_bea_period(.data$time_period),
        value = .data$data_value
      ) |>
      dplyr::select(.data$date_frequency, .data$date, .data$value)

    tibble::tibble(
      series_id = series_id,
      series_title = unique(group$industry_description),
      metadata = list(metadata_tibble),
      data = list(data_only)
    )
  })

  # Add series names if industry codes are named
  complete_results = add_series_names(complete_results, industry)

  if (metadata) {
    complete_results
  } else {
    extract_data(complete_results, bea_industry_data_extractor)
  }
}

#' @noRd
bea_industry_data_extractor = function(series_id, complete_results) {
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
