#' Handle BEA API error responses
#'
#' Checks for errors in BEA API response and throws structured error if found
#'
#' @param result Parsed JSON response from the BEA API
#' @noRd
handle_bea_api_error = function(result) {
  # Check for API errors
  if (!is.null(result$BEAAPI$Error)) {
    error_info = result$BEAAPI$Error

    # Extract all error components
    error_description = error_info$APIErrorDescription %||% "Unknown error"
    error_code = error_info$APIErrorCode %||% NA_character_
    error_detail = error_info$ErrorDetail$Description %||% NA_character_

    # Build comprehensive error message
    error_msg = paste0("BEA API error: ", error_description)

    if (!is.na(error_code)) {
      error_msg = paste0(error_msg, " (Error code: ", error_code, ")")
    }

    if (!is.na(error_detail)) {
      error_msg = paste0(error_msg, "\n", error_detail)
    }

    # Use rlang::abort() for structured error handling
    rlang::abort(
      error_msg,
      class = "bea_api_error",
      error_code = error_code,
      error_description = error_description,
      error_detail = error_detail
    )
  }
}

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
  validate_api_key(
    bea_api_key,
    "BEA",
    "https://www.bea.gov/API/signup/index.cfm"
  )

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
  handle_bea_api_error(result)

  return(result)
}

# Helper functions for data processing ----------------------------------------

#' Extract all note texts from BEA API notes
#' @param raw_notes List of note objects from BEA API response
#' @returns Character vector of note texts
#' @noRd
extract_all_note_texts = function(raw_notes) {
  if (is.null(raw_notes) || length(raw_notes) == 0) {
    return(character(0))
  }
  purrr::map_chr(raw_notes, ~ .x$NoteText %||% NA_character_)
}

#' Extract table description from BEA API notes
#' @param raw_notes List of note objects from BEA API response
#' @param table_name Table name to match in NoteRef field
#' @returns Character string with table description or NA
#' @noRd
extract_table_description = function(raw_notes, table_name) {
  if (is.null(raw_notes) || length(raw_notes) == 0) {
    return(NA_character_)
  }

  for (note in raw_notes) {
    if (!is.null(note$NoteRef) && note$NoteRef == table_name) {
      return(note$NoteText %||% NA_character_)
    }
  }

  NA_character_
}

#' Parse BEA data value string to numeric
#' @param value_string Character string with possible comma separators
#' @returns Numeric value
#' @noRd
parse_data_value = function(value_string) {
  as.numeric(gsub(",", "", value_string %||% NA_character_))
}

#' Convert a single NIPA API row to a tibble row
#' @param row List representing one row from BEA NIPA API
#' @param table_description Description of the table
#' @param all_note_texts Character vector of all note texts
#' @returns One-row tibble with standardized columns
#' @noRd
convert_nipa_row_to_tibble = function(row, table_description, all_note_texts) {
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
    note_ref = row$NoteRef %||% NA_character_,
    note_text = list(all_note_texts)
  )
}

#' Process dates and add frequency/period columns to NIPA data
#' @param data_tibble Tibble with time_period and data_value columns
#' @returns Tibble with added date, frequency, and period columns
#' @noRd
process_nipa_dates_and_values = function(data_tibble) {
  data_tibble |>
    dplyr::mutate(
      date_frequency = normalize_api_frequency(.data$time_period, "bea"),
      date = parse_api_date(.data$time_period),
      year = extract_period_component(.data$time_period, "year"),
      quarter = extract_period_component(.data$time_period, "quarter"),
      month = extract_period_component(.data$time_period, "month"),
      value = parse_data_value(.data$data_value)
    )
}

#' Extract line number and description from Regional API data
#' @param code Code string from API (e.g., "SAINC1-2")
#' @param description Description from API row
#' @param line_code Original line_code parameter
#' @param statistic Statistic description from API results
#' @returns Named list with line_number and line_description
#' @noRd
extract_regional_line_info = function(code, description, line_code, statistic) {
  # When line_code is "ALL", extract from the row's Code field
  if (is.character(line_code) && toupper(line_code) == "ALL") {
    code_parts = strsplit(code %||% "", "-")[[1]]
    list(
      line_number = as.integer(code_parts[length(code_parts)]),
      line_description = description %||% NA_character_
    )
  } else {
    # When specific line_code, use the parameter and statistic
    list(
      line_number = as.integer(line_code),
      line_description = statistic
    )
  }
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
#' @examplesIf nzchar(Sys.getenv("BEA_API_KEY"))
#' get_bea_nipa("T10101", years = 2020:2024, frequency = "quarter")
#'
#' get_bea_nipa(
#'   c("gdp" = "T10101", "personal_income" = "T20305"),
#'   years = 2023:2024,
#'   frequency = "year"
#' )
#'
#' get_bea_nipa("T10101", years = 2023, frequency = "year", metadata = TRUE)
get_bea_nipa = function(
  tables,
  years,
  frequency = c("year", "quarter", "month"),
  underlying = FALSE,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
) {
  # Convert frequency to BEA API codes (A=Annual, Q=Quarterly, M=Monthly)
  frequency = match.arg(frequency, several.ok = TRUE)
  freq_code = dplyr::case_match(
    frequency,
    "year" ~ "A",
    "quarter" ~ "Q",
    "month" ~ "M"
  )
  freq_param = paste(freq_code, collapse = ",")

  # Convert years to API parameter format
  if (is.character(years) && toupper(years) == "ALL") {
    years_param = "ALL"
  } else {
    years_param = paste(years, collapse = ",")
  }

  # Select dataset (NIPA or underlying detail tables)
  dataset_name = if (underlying) "NIUnderlyingDetail" else "NIPA"

  # Fetch data for each table and combine results
  all_results = seq_along(tables) |>
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

  # Add custom names if tables parameter was a named vector
  table_names = names(tables)
  if (!is.null(table_names)) {
    all_results = all_results |>
      dplyr::mutate(
        name = table_names[match(.data$table_name, tables)],
        .before = 1
      ) |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "", NA_character_, .data$name)
      )
  }

  # Determine which date columns to include based on actual data frequencies
  unique_frequencies = unique(all_results$date_frequency)
  date_cols = c("date", "year")

  if (any(c("quarter", "month") %in% unique_frequencies)) {
    date_cols = c(date_cols, "quarter")
  }

  if ("month" %in% unique_frequencies) {
    date_cols = c(date_cols, "month")
  }

  # Return results with appropriate columns
  if (metadata) {
    all_results
  } else {
    all_results |>
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

  # Extract data and notes from API response
  raw_data = result$BEAAPI$Results$Data
  raw_notes = result$BEAAPI$Results$Notes

  # Extract metadata from notes
  table_description = extract_table_description(raw_notes, table_name)
  all_note_texts = extract_all_note_texts(raw_notes)

  # Convert each row to a tibble and combine
  data_tibble = purrr::map_dfr(
    raw_data,
    ~ convert_nipa_row_to_tibble(.x, table_description, all_note_texts)
  )

  # Process dates and values, then select final columns
  process_nipa_dates_and_values(data_tibble) |>
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

#' Process BEA series groups into structured result
#'
#' Consolidates the common pattern of grouping and processing BEA data
#' for the Industry dataset.
#'
#' @param data_tibble Tibble containing raw BEA data
#' @param group_vars Character vector of column names to group by
#' @param series_id_cols Character vector of column names to construct series_id
#' @param series_title_fn Function to construct series_title from group
#' @param metadata_cols Character vector of column names for metadata
#' @param use_bea_dates Logical flag to use BEA date parsing
#'
#' @returns Tibble with series_id, series_title, metadata (list), data (list)
#' @noRd
process_bea_series_groups = function(
  data_tibble,
  group_vars,
  series_id_cols,
  series_title_fn,
  metadata_cols,
  use_bea_dates = TRUE
) {
  # Group by unique series identifiers
  series_groups = data_tibble |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::group_split()

  # Process each series group
  purrr::map_dfr(series_groups, function(group) {
    # Construct series_id by joining specified columns with underscore
    series_id_parts = purrr::map_chr(series_id_cols, ~ unique(group[[.x]]))
    series_id = paste(series_id_parts, collapse = "_")

    # Construct series_title using provided function
    series_title = series_title_fn(group)

    # Create metadata tibble
    metadata_tibble = group |>
      dplyr::select(dplyr::all_of(metadata_cols)) |>
      dplyr::distinct()

    # Create data tibble with BEA date handling
    data_only = group |>
      dplyr::mutate(
        date_frequency = normalize_api_frequency(.data$time_period, "bea"),
        date = parse_api_date(.data$time_period),
        value = .data$data_value
      ) |>
      dplyr::select(.data$date_frequency, .data$date, .data$value)

    tibble::tibble(
      series_id = series_id,
      series_title = series_title,
      metadata = list(metadata_tibble),
      data = list(data_only)
    )
  })
}

#' Retrieve Regional data from the BEA API
#'
#' Retrieves regional economic data from the
#' \href{https://apps.bea.gov/API/signup/}{Bureau of Economic Analysis API}.
#' Requires a BEA API key saved in the `BEA_API_KEY` environment variable.
#'
#' @param geo_fips Character vector of geographic FIPS codes, or special values:
#'   "COUNTY" for all counties, "STATE" for all states, "MSA" for all MSAs.
#'   A single value can also be a state abbreviation (e.g., "NY", "CA").
#' @param table_name Single table name (e.g., "CAINC1" for personal income)
#' @param line_code Single line code specifying the statistic to retrieve, or "ALL" for all line codes
#' @param year Optional numeric vector of years or "ALL" for all available years.
#'   Defaults to "LAST5".
#' @param metadata Logical flag to return additional metadata columns (default: FALSE).
#'   When TRUE, also includes cl_unit, mult_unit, and note_text (list column).
#' @param bea_api_key BEA API key (defaults to BEA_API_KEY environment variable)
#'
#' @returns A tibble with columns: geo_fips, geo_name, table_name,
#'   table_description, line_number, line_description, date_frequency, date, year, value.
#'   If metadata = TRUE, also includes cl_unit, mult_unit, and note_text (list column
#'   containing all notes from the API response).
#'
#' @export
#' @examplesIf nzchar(Sys.getenv("BEA_API_KEY"))
#' get_bea_regional(geo_fips = "STATE", table_name = "SAINC1", line_code = 1)
#'
#' get_bea_regional(geo_fips = c("36000", "06000"), table_name = "SAINC1", line_code = 1)
get_bea_regional = function(
  geo_fips,
  table_name,
  line_code,
  year = NULL,
  metadata = FALSE,
  bea_api_key = Sys.getenv("BEA_API_KEY")
) {
  # Normalize year parameter
  if (is.null(year)) {
    years_param = "LAST5"
  } else if (is.character(year) && toupper(year) == "ALL") {
    years_param = "ALL"
  } else {
    years_param = paste(year, collapse = ",")
  }

  # Iterate over each geo_fips/line_code combination
  combinations = tidyr::expand_grid(
    geo = geo_fips,
    line = line_code
  )

  results = purrr::map2(
    combinations$geo,
    combinations$line,
    function(geo, line) {
      fetch_bea_regional(
        geo_fips = geo,
        table_name = table_name,
        line_code = line,
        years_param = years_param,
        bea_api_key = bea_api_key
      )
    }
  ) |>
    purrr::list_rbind()

  # Sort by geo_fips, table_name, line_number, date
  results = results |>
    dplyr::arrange(
      .data$geo_fips,
      .data$table_name,
      .data$line_number,
      .data$date
    )

  # Select columns based on metadata flag
  if (metadata) {
    results
  } else {
    results |>
      dplyr::select(
        .data$geo_fips,
        .data$geo_name,
        .data$table_name,
        .data$table_description,
        .data$line_number,
        .data$line_description,
        .data$date_frequency,
        .data$date,
        .data$year,
        .data$value
      )
  }
}

#' Fetch BEA Regional data for a single geo_fips/line_code combination
#' @noRd
fetch_bea_regional = function(
  geo_fips,
  table_name,
  line_code,
  years_param,
  bea_api_key
) {
  # Make API call
  result = get_bea_api(
    dataset_name = "Regional",
    params = list(
      GeoFips = geo_fips,
      TableName = table_name,
      LineCode = line_code,
      Year = years_param
    ),
    bea_api_key = bea_api_key
  )

  # Extract data and metadata from API response
  raw_data = result$BEAAPI$Results$Data
  raw_notes = result$BEAAPI$Results$Notes
  public_table = result$BEAAPI$Results$PublicTable %||% NA_character_
  statistic = result$BEAAPI$Results$Statistic %||% NA_character_

  # Extract all note texts
  all_note_texts = extract_all_note_texts(raw_notes)

  # Convert each row to tibble
  purrr::map_dfr(raw_data, function(row) {
    # Extract line information based on line_code parameter
    line_info = extract_regional_line_info(
      row$Code,
      row$Description,
      line_code,
      statistic
    )

    time_period = row$TimePeriod %||% NA_character_

    tibble::tibble(
      geo_fips = row$GeoFips %||% NA_character_,
      geo_name = row$GeoName %||% NA_character_,
      table_name = table_name,
      table_description = public_table,
      line_number = line_info$line_number,
      line_description = line_info$line_description,
      date_frequency = normalize_api_frequency(time_period, "bea"),
      date = parse_api_date(time_period),
      year = extract_period_component(time_period, "year"),
      value = parse_data_value(row$DataValue),
      cl_unit = row$CL_UNIT %||% NA_character_,
      mult_unit = row$UNIT_MULT %||% NA_character_,
      note_text = list(all_note_texts)
    )
  })
}
