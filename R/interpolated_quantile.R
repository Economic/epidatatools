#' interpolated_quantile: binned interpolated quantile
#' @param x numeric vector or an R object
#' @param bin_size size used for binning
#' @param probs numeric; percentile with value [0,1]
#' @param w numeric vector of weights the same length as x giving the weights to use for elements of x
#' @param na.rm logical; if true, any NA or NaN's are removed from x before computation
#'
#' @return a numeric vector of length 1
#' @export
#'
#' @examples interpolated_quantile(x = mtcars[["cyl"]], bin_size = 0.25)
#' @examples mtcars |> dplyr::summarise(value = interpolated_quantile(x = cyl, bin_size = 0.25, probs = 0.75, w = mpg))
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
#' @param data data frame, data frame extension (e.g. a tibble), or a lazy data frame (dbplyr, dtplyr) inherited from tidyverse
#' @param p numeric; percentile with value [0,1]
#'
#' @return a numeric vector of length 1
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
#' @param x numeric vector or an R object
#' @param bin_size size used for binning
#' @param w numeric vector of weights the same length as x giving the weights to use for elements of x
#' @param na.rm logical; if true, any NA or NaN's are removed from x before computation
#'
#' @return a tibble or data frame
#' @export
#'
#' @examples interpolated_median(x = mtcars[["cyl"]], bin_size = 0.25)
interpolated_median = function(x, bin_size, w = NULL, na.rm = TRUE) {
  interpolated_quantile(x, bin_size = bin_size, probs = 0.5, w = w, na.rm = na.rm)
}

#' Binipolate
#' Calculates binned interpolated percentiles
#'
#' @param data data frame, data frame extension (e.g. a tibble), or a lazy data frame (dbplyr, dtplyr) inherited from tidyverse
#' @param x column to compute
#' @param probs numeric vector of percentiles with values [0,1]
#' @param bin_size size of binning
#' @param .by optional, a tidy-selection of columns for single-operation grouping
#' @param w numeric vector of weights the same length as x giving the weights to use for elements of x
#'
#' @return a tibble or data frame
#' @export
#' @examples binipolate(mtcars, x = cyl, binsize = 0.25)
#' @examples mtcars |> binipolate(disp, probs = (0.25, 0.5, 0.75), binsize = 0.25)
#' @examples mtcars |> binipolate(disp, probs = (0.25, 0.5, 0.75), binsize = 0.25, .by = cyl, w = mpg)
binipolate = function(data, x, probs = 0.5, bin_size, .by = NULL, w = NULL) {
  data |>
    group_by(pick({{.by}})) |>
    dplyr::reframe(
      probs = probs,
      value = interpolated_quantile(
        {{x}},
        bin_size = bin_size,
        probs = probs,
        w = {{w}},
      )
    ) |>
    ungroup()
}

