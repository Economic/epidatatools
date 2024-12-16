#' Calculate the averaged (smoothed) quantile
#'
#' @param x numeric vector or an R object
#' @param w numeric vector of sample weights the same length as x giving the weights to use for elements of x
#' @param probs numeric; percentile with value `[0,1]`
#' @param na.rm logical; if true, any NA or NaN's are removed from x before computation
#' @param quantiles_n integer number of quantiles used for averaging; must be odd
#' @param quantiles_w weights used for average quantiles; length must equal quantiles_n
#'
#' @return a numeric vector with length probs
#' @export
#'
#' @examples averaged_quantile(x = mtcars$mpg, probs = c(0.25, 0.5, 0.75))
averaged_quantile = function(
    x,
    w = NULL,
    probs = 0.5,
    na.rm = TRUE,
    quantiles_n = 9L,
    quantiles_w = c(1:4, 5, 4:1)) {

  if (is.null(w)) w = 1

  averaged_quantile_arg_check(x, w, probs, quantiles_n, quantiles_w)

  # return NA output
  if (any(is.na(x)) & na.rm == FALSE) {
    output = rep(NA_real_, length(probs))
  }

  else {
    data = tibble::tibble(x = x, w = w) |>
      tidyr::drop_na()

    output = probs |>
      purrr::map(~ averaged_quantile_p(data, .x, quantiles_n, quantiles_w)) |>
      purrr::list_c()
  }

  output

}

#' Calculate the averaged (smoothed) median
#'
#' @param x numeric vector or an R object
#' @param w numeric vector of sample weights the same length as x giving the weights to use for elements of x
#' @param na.rm logical; if true, any NA or NaN's are removed from x before computation
#' @param quantiles_n integer number of quantiles used for averaging; must be odd
#' @param quantiles_w weights used for average quantiles; length must equal quantiles_n
#'
#' @return a scalar
#' @export
#'
#' @examples averaged_median(x = mtcars$mpg)
averaged_median = function(
    x,
    w = NULL,
    na.rm = TRUE,
    quantiles_n = 9L,
    quantiles_w = c(1:4, 5, 4:1)) {

  averaged_quantile(x, w, probs = 0.5, na.rm, quantiles_n, quantiles_w)

}

# averaged_quantile helper
averaged_quantile_p = function(data, p, quantiles_n, quantiles_w) {

  extra_prob_limit = (quantiles_n - 1) / 2

  probs = p + seq(-extra_prob_limit, extra_prob_limit, 1) / 100

  data |>
    dplyr::reframe(
      probs,
      quantiles_w,
      value = MetricsWeighted::weighted_quantile(.data[["x"]], .data[["w"]], probs)
    ) |>
    dplyr::summarize(stats::weighted.mean(value, w = quantiles_w)) |>
    dplyr::pull()
}

# check arguments for averaged_quantile
averaged_quantile_arg_check = function(x, w, probs, quantiles_n, quantiles_w) {
  stopifnot(
    all(w >= 0),
    quantiles_n %% 2 == 1,
    all(quantiles_w >= 0),
    sum(quantiles_w) > 0,
    length(quantiles_w) == quantiles_n,
    length(x) >= 1L,
    all(probs > 0),
    all(probs < 1)
  )
}
