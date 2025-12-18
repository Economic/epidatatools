#' Calculate the binned interpolated quantile
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is superceded by EPI's current preferred
#' method for interpolating quantiles, [averaged_quantile()].
#'
#' @param x numeric vector or an R object
#' @param bin_size size used for binning
#' @param probs numeric; percentile with value `[0,1]`
#' @param w numeric vector of weights the same length as x giving the weights to use for elements of x
#' @param na.rm logical; if true, any NA or NaN's are removed from x before computation
#'
#' @return a numeric vector
#' @export
#'
#' @examples interpolated_quantile(x = mtcars$mpg, bin_size = 0.50, probs = c(0.25, 0.5, 0.75))
interpolated_quantile = function(
  x,
  bin_size,
  probs = 0.5,
  w = NULL,
  na.rm = TRUE
) {
  if (is.null(w)) {
    w = 1
  }

  stopifnot(
    bin_size > 0,
    all(w >= 0),
    length(x) >= 1L,
    all(probs > 0),
    all(probs < 1)
  )

  if (any(is.na(x)) & na.rm == FALSE) {
    output = rep(NA_real_, length(probs))
  } else {
    data = tibble::tibble(x = x, w = w) |>
      tidyr::drop_na() |>
      # assign a value to bin
      # example: with bin size 0.25 we want $25 to be assigned value $25.125
      # later we will interpolate between this bin value and the bin value below it ($24.875)
      dplyr::mutate(
        bin_value = floor((x - bin_size / 2) / bin_size) *
          bin_size +
          bin_size +
          bin_size / 2
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

# interpolated quantile helper
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
    dplyr::mutate(
      value = bin_value +
        (dplyr::lead(bin_value) - bin_value) *
          ((p - cdf) / (dplyr::lead(cdf) - cdf))
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::pull(value)
}

#' Calculate the binned interpolated median
#'
#' #' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is superceded by EPI's current preferred
#' method for interpolating medians, [averaged_median()].
#'
#' @param x numeric vector or an R object
#' @param bin_size size used for binning
#' @param w numeric vector of weights the same length as x giving the weights to use for elements of x
#' @param na.rm logical; if true, any NA or NaN's are removed from x before computation
#'
#' @return numeric vector
#' @export
#'
#' @examples interpolated_median(x = mtcars$mpg, bin_size = 0.50)
interpolated_median = function(x, bin_size, w = NULL, na.rm = TRUE) {
  interpolated_quantile(
    x,
    bin_size = bin_size,
    probs = 0.5,
    w = w,
    na.rm = na.rm
  )
}

#' Summarize a data frame as binned interpolated percentiles
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function was superseded and EPI insteads uses
#' [averaged_quantile()] to interpolate percentiles.
#'
#' @param data data frame
#' @param x column to compute
#' @param probs numeric vector of percentiles with values `[0,1]`
#' @param bin_size size of binning
#' @param .by optional, a tidy-selection of columns for single-operation grouping
#' @param w numeric vector of weights the same length as x giving the weights to use for elements of x
#'
#' @return a tibble or data frame
#' @export
#' @examples binipolate(mtcars, mpg, bin_size = 0.25)
#' @examples binipolate(mtcars, mpg, probs = c(0.25, 0.5, 0.75), bin_size = 0.25)
#' @examples binipolate(mtcars, mpg, probs = c(0.25, 0.5, 0.75), bin_size = 0.25, .by = cyl, w = wt)
binipolate = function(data, x, probs = 0.5, bin_size, .by = NULL, w = NULL) {
  data |>
    dplyr::group_by(dplyr::pick({{ .by }})) |>
    dplyr::reframe(
      probs = probs,
      value = interpolated_quantile(
        {{ x }},
        bin_size = bin_size,
        probs = probs,
        w = {{ w }},
      )
    ) |>
    dplyr::ungroup()
}
