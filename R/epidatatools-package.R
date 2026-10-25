#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  ".env", "bin_value", "bin_number", "cdf", "name", "value",
  # find_bls(): catalog columns used in dplyr and arrow expressions
  ".hits", "n", "program", "seasonal", "series_title", "survey_aliases", "survey_name"
))

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom rlang %||%
## usethis namespace: end
NULL
