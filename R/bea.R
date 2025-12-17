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
      date_frequency = normalize_api_frequency(.data$time_period, "bea"),
      date = parse_api_date(.data$time_period),
      year = extract_period_component(.data$time_period, "year"),
      quarter = extract_period_component(.data$time_period, "quarter"),
      month = extract_period_component(.data$time_period, "month"),
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
#' # Get personal income for all states
#' get_bea_regional(geo_fips = "STATE", table_name = "SAINC1", line_code = 1)
#'
#' # Get data for specific states
#' get_bea_regional(geo_fips = c("36000", "06000"), table_name = "SAINC1", line_code = 1)
#'
#' # Use state abbreviation for single state
#' get_bea_regional(geo_fips = "NY", table_name = "SAINC1", line_code = 1)
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

  # Extract data and metadata from response
  raw_data = result$BEAAPI$Results$Data
  raw_notes = result$BEAAPI$Results$Notes
  public_table = result$BEAAPI$Results$PublicTable %||% NA_character_
  statistic = result$BEAAPI$Results$Statistic %||% NA_character_

  # Collect all note texts for the note_text list column
  all_note_texts = if (!is.null(raw_notes) && length(raw_notes) > 0) {
    purrr::map_chr(raw_notes, ~ .x$NoteText %||% NA_character_)
  } else {
    character(0)
  }

  # Determine if we need to extract line info from each row (when line_code is "ALL")
  use_row_line_info = is.character(line_code) && toupper(line_code) == "ALL"

  # Convert to tibble
  purrr::map_dfr(raw_data, function(row) {
    # Extract line number and description from row when using "ALL"
    # Code field format is "TableName-LineNumber" (e.g., "SAINC1-2")
    if (use_row_line_info) {
      code_parts = strsplit(row$Code %||% "", "-")[[1]]
      row_line_number = as.integer(code_parts[length(code_parts)])
      row_line_description = row$Description %||% NA_character_
    } else {
      row_line_number = as.integer(line_code)
      row_line_description = statistic
    }

    time_period = row$TimePeriod %||% NA_character_

    tibble::tibble(
      geo_fips = row$GeoFips %||% NA_character_,
      geo_name = row$GeoName %||% NA_character_,
      table_name = table_name,
      table_description = public_table,
      line_number = row_line_number,
      line_description = row_line_description,
      date_frequency = normalize_api_frequency(time_period, "bea"),
      date = parse_api_date(time_period),
      year = extract_period_component(time_period, "year"),
      value = as.numeric(gsub(",", "", row$DataValue %||% NA_character_)),
      cl_unit = row$CL_UNIT %||% NA_character_,
      mult_unit = row$UNIT_MULT %||% NA_character_,
      note_text = list(all_note_texts)
    )
  })
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

  # Process series groups using consolidated helper
  complete_results = process_bea_series_groups(
    data_tibble,
    group_vars = c("table_id", "industry", "industry_description"),
    series_id_cols = c("table_id", "industry"),
    series_title_fn = function(group) {
      unique(group$industry_description)
    },
    metadata_cols = c(
      "table_id",
      "industry",
      "industry_description",
      "cl_unit",
      "unit_mult"
    ),
    use_bea_dates = TRUE # Uses BEA frequency normalization
  )

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
  generic_data_extractor(
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
    )
  )
}
