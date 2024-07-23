#' interpolated_quantile: binned interpolated quantiles
#' @param x
#' @param bin_size
#' @param probs
#' @param w
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
interpolated_quantile = function(x, bin_size, probs = 0.5, w = NULL, na.rm = TRUE) {

  if (is.null(w)) w = 1

  stopifnot(
    bin_size > 0,
    all(w >= 0),
    length(x) >= 1L,
    all(probs > 0),
    all(probs < 1)
  )

  if (any(is.na(x)) & na.rm == FALSE) {
    output = rep(NA_real_, length(probs))
  }

  else {
    data = tibble::tibble(x = x, w = w) |>
      tidyr::drop_na() |>
      # assign a value to bin
      # example: with bin size 0.25 we want $25 to be assigned value $25.125
      # later we will interpolate between this bin value and the bin value below it ($24.875)
      dplyr::mutate(
        bin_value = floor((x - bin_size/2)/bin_size) * bin_size + bin_size + bin_size/2
      ) |>
      dplyr::summarise(sum = sum(w), .by = bin_value) |>
      dplyr::arrange(bin_value) |>
      dplyr::mutate(cdf = cumsum(sum) / sum(sum)) |>
      dplyr::mutate(bin_number = dplyr::row_number())

    output = probs |>
      purrr::map(~ interpolated_quantile_p(data, .x)) |>
      purrr::list_c()
  }

  output

}

#' interpolated_quantile helper
#'
#' @param data
#' @param p
#'
#' @return
#'
#' @examples
interpolated_quantile_p = function(data, p) {
  # identify the highest bin that doesn't contain p
  below = data |>
    dplyr::filter(cdf < p) |>
    dplyr::filter(bin_number == max(bin_number)) |>
    dplyr::pull(bin_number)

  data |>
    # grab two bins just below p and including p
    dplyr::filter(bin_number %in% c(below, below + 1)) |>
    dplyr::arrange(bin_number) |>
    # interpolate (implicitly assuming uniform distribution within bin)
    dplyr::mutate(value = bin_value + (dplyr::lead(bin_value) - bin_value) * ((p - cdf) / (dplyr::lead(cdf) - cdf))) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::pull(value)
}

#' interpolated_median: binned interpolated median
#'
#' @param x
#' @param bin_size
#' @param w
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
interpolated_median = function(x, bin_size, w = NULL, na.rm = TRUE) {
  interpolated_quantile(x, bin_size = bin_size, probs = 0.5, w = w, na.rm = na.rm)
}

#' Binipolate
#' Calculates binned interpolated percentiles
#' If classical = TRUE, uses [MetricsWeighted::weighted_quantile()]
#'
#' @param data a data frame
#' @param var unquoted variable for analysis
#' @param probs percentiles to calculate
#' @param bin_size size of bin
#' @param .by tidy selection of grouping variable
#' @param w optional weight in data
#' @return a tibble
#' @export
#' @importFrom dplyr %>%
#' @examples binipolate(mtcars, var = "cyl", binsize = 0.25)
#' @examples binipolate(mtcars, var = "gear", w = wt, binsize = 0.25)
#' @examples binipolate(mtcars, var = "mpg", group_vars = c("cyl", "gear"), w = wt, binsize = 0.25)
binipolate = function(data, var, probs = 0.5, bin_size, .by = NULL, w = NULL) {
  data |>
    group_by(pick({{.by}})) |>
    dplyr::reframe(
      probs = probs,
      value = interpolated_quantile(
        {{var}},
        bin_size = bin_size,
        probs = probs,
        w = {{w}},
      )
    ) |>
    ungroup()
}

