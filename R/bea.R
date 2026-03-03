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
    # Note: ErrorDetail is assumed to be a single JSON object (not an array).
    # If the BEA API ever returns it as an array, simplifyVector = TRUE would
    # convert it to a data frame and the is.na() check below would fail.
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
  result = jsonlite::fromJSON(content, simplifyVector = TRUE)

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
  if (is.null(raw_notes) || !is.data.frame(raw_notes) || nrow(raw_notes) == 0) {
    return(character(0))
  }
  raw_notes$NoteText
}

#' Extract table description from BEA API notes
#' @param raw_notes List of note objects from BEA API response
#' @param table_name Table name to match in NoteRef field
#' @returns Character string with table description or NA
#' @noRd
extract_table_description = function(raw_notes, table_name) {
  if (is.null(raw_notes) || !is.data.frame(raw_notes) || nrow(raw_notes) == 0) {
    return(NA_character_)
  }

  match_idx = which(raw_notes$NoteRef == table_name)
  if (length(match_idx) == 0) {
    return(NA_character_)
  }

  raw_notes$NoteText[match_idx[1]]
}

#' Parse BEA data value string to numeric
#' @param value_string Character string with possible comma separators
#' @returns Numeric value
#' @noRd
parse_data_value = function(value_string) {
  as.numeric(gsub(",", "", value_string %||% NA_character_))
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
  freq_code = dplyr::recode_values(
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

  # Construct tibble directly from data frame columns
  data_tibble = tibble::tibble(
    table_name       = raw_data$TableName,
    table_description = table_description,
    series_code      = raw_data$SeriesCode,
    line_number      = as.integer(raw_data$LineNumber),
    line_description = raw_data$LineDescription,
    time_period      = raw_data$TimePeriod,
    metric_name      = raw_data$METRIC_NAME,
    cl_unit          = raw_data$CL_UNIT,
    unit_mult        = as.integer(raw_data$UNIT_MULT),
    data_value       = raw_data$DataValue,
    note_ref         = raw_data$NoteRef,
    note_text        = list(all_note_texts)
  )

  # Process dates and values, then select final columns
  process_nipa_dates_and_values(data_tibble) |>
    dplyr::select(
      "table_name",
      "table_description",
      "line_number",
      "line_description",
      "date_frequency",
      "date",
      "year",
      "quarter",
      "month",
      "value",
      "unit_mult",
      "metric_name",
      "cl_unit",
      "series_code",
      "note_text"
    )
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
      .data[["geo_fips"]],
      .data[["table_name"]],
      .data[["line_number"]],
      .data[["date"]]
    )

  # Select columns based on metadata flag
  if (metadata) {
    results
  } else {
    results |>
      dplyr::select(
        "geo_fips",
        "geo_name",
        "table_name",
        "table_description",
        "line_number",
        "line_description",
        "date_frequency",
        "date",
        "year",
        "value"
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

  # Extract line information based on line_code parameter
  if (is.character(line_code) && toupper(line_code) == "ALL") {
    line_numbers = as.integer(sub(".*-", "", raw_data$Code))
    line_descriptions = raw_data$Description
  } else {
    line_numbers = as.integer(line_code)
    line_descriptions = statistic
  }

  # Construct tibble directly from data frame columns
  tibble::tibble(
    geo_fips         = raw_data$GeoFips,
    geo_name         = raw_data$GeoName,
    table_name       = table_name,
    table_description = public_table,
    line_number      = line_numbers,
    line_description = line_descriptions,
    date_frequency   = normalize_api_frequency(raw_data$TimePeriod, "bea"),
    date             = parse_api_date(raw_data$TimePeriod),
    year             = extract_period_component(raw_data$TimePeriod, "year"),
    value            = parse_data_value(raw_data$DataValue),
    cl_unit          = raw_data$CL_UNIT,
    mult_unit        = raw_data$UNIT_MULT,
    note_text        = list(all_note_texts)
  )
}
