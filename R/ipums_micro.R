#' Download a selection of IPUMS microdata extracts
#'
#' Convenience functions for downloading samples and variables from \href{https://www.ipums.org/}{IPUMS} microdata using their \href{https://developer.ipums.org/}{API} and package \href{https://tech.popdata.org/ipumsr/articles/ipums-api-micro.html}{`ipumsr`}.
#'
#' These functions are simply wrappers around \href{https://tech.popdata.org/ipumsr/}{`ipumsr`} and require you to have an \href{https://tech.popdata.org/ipumsr/articles/ipums-api.html#set-key}{IPUMS API key} saved in the `IPUMS_API_KEY` environment variable.
#'
#' @describeIn ipums_micro base function group
#' @param extract an IPUMS microdata extract as defined by [ipumsr::define_extract_micro()]
#' @return a tibble of microdata from the IPUMS API
#' @export
#' @examplesIf FALSE
#' extract = ipumsr::define_extract_micro(
#'   collection = "cps",
#'   description = "CPS ASEC extract",
#'   samples = c("cps2021_03s", "cps2022_03s", "cps2023_03s"),
#'   variables = c("YEAR", "OFFPOV", "WTSUPP")
#' )
#' dl_ipums_micro(extract)
#'
#' dl_ipums_asec(2021:2023, c("YEAR", "OFFPOV", "WTSUPP"))
#'
#' begin_month = lubridate::ym("2022 September")
#' end_month = lubridate::ym("2024 August")
#' cps_months = seq(begin_month, end_month, by = "month")
#' dl_ipums_cps(cps_months, c("EARNWT", "HOURWAGE2"))
dl_ipums_micro = function(extract) {
  extract_submitted = ipumsr::submit_extract(extract)
  extract_complete = ipumsr::wait_for_extract(extract_submitted)
  path = ipumsr::download_extract(extract_submitted, download_dir = tempdir())
  ddi = ipumsr::read_ipums_ddi(path)
  micro_data = ipumsr::read_ipums_micro(ddi)

  micro_data
}

#' @export
#' @describeIn ipums_micro Download IPUMS ACS 1-year files
#' @param variables a vector of variable names, or a list of detailed variable specifications as created by [ipumsr::var_spec()]
#' @param years a vector of years
#' @param description description for the extract
dl_ipums_acs1 = function(
    years = NULL,
    variables,
    description = NULL) {

  if (is.null(description)) description = "ACS extract"

  extract = refine_ipums_extract(
    collection = "usa",
    sample_filter = "\\d{4} ACS",
    dates = years,
    variables = variables,
    frequency = "annual",
    description = description
  )

  dl_ipums_micro(extract)
}

#' @export
#' @describeIn ipums_micro Download IPUMS CPS ASEC
dl_ipums_asec = function(
    years = NULL,
    variables,
    description = NULL) {

  if (is.null(description)) description = "CPS ASEC extract"

  extract = refine_ipums_extract(
    collection = "cps",
    sample_filter = "ASEC",
    dates = years,
    variables = variables,
    frequency = "annual",
    description = description
  )

  dl_ipums_micro(extract)
}

#' @export
#' @describeIn ipums_micro Download IPUMS Monthly CPS
#' @param months a vector of dates representing months of CPS samples.
dl_ipums_cps = function(
    months = NULL,
    variables,
    description = NULL) {

  if (is.null(description)) description = "CPS Monthly"

  extract = refine_ipums_extract(
    collection = "cps",
    sample_filter = "IPUMS-CPS",
    dates = months,
    variables = variables,
    frequency = "monthly",
    description = description
  )

  dl_ipums_micro(extract)
}


refine_ipums_extract = function(
    collection,
    sample_filter,
    dates,
    variables,
    frequency,
    description) {

  # refine collection to appropriate samples
  all_samples = ipumsr::get_sample_info(collection) |>
    dplyr::filter(stringr::str_detect(description, sample_filter))

  if (frequency == "annual") {
    all_samples = all_samples |>
      dplyr::mutate(date = stringr::str_extract(name, "\\d{4}"))

  }
  else if (frequency == "monthly" & collection == "cps") {
    all_samples = all_samples |>
      dplyr::filter(stringr::str_detect(description, "ASEC", negate = TRUE)) |>
      dplyr::mutate(
        date = stringr::word(description, start = -2, end = -1),
        date = lubridate::my(date)
      )
  }
  else stop("Unable to parse collection and/or dates")

  all_samples = dplyr::select(all_samples, name, date)

  # use all available dates if not specified
  if (is.null(dates)) dates = dplyr::pull(all_samples, date)

  # if monthly ensure dates have day = 1
  if (frequency == "monthly") dates = lubridate::ym(format(dates, "%Y %B"))

  dates = unique(dates)

  # filter samples only to requested dates
  valid_samples = all_samples |>
      dplyr::filter(date %in% dates)

  sample_names = valid_samples |>
    dplyr::pull(name)

  # checks
  valid_dates = valid_samples |>
    dplyr::pull(date) |>
    unique()

  if(length(dates) != length(valid_dates)) {
    warning(
      "Available IPUMS time periods differ from periods requested.",
      "\nOnly downloading IPUMS samples ", paste(sample_names, collapse = ", ")
    )
  }

  extract = ipumsr::define_extract_micro(
    collection = collection,
    description = description,
    samples = sample_names,
    variables = variables
  )

  extract
}


