#' Deprecated IPUMS download functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were renamed in epidatatools 2.0.0 to match the package's other
#' data functions, such as [get_bls()] and [get_fred()]:
#'
#' * `dl_ipums_micro()` is now [get_ipums_micro()]
#' * `dl_ipums_acs1()` is now [get_ipums_acs1()]
#' * `dl_ipums_asec()` is now [get_ipums_asec()]
#' * `dl_ipums_cps()` is now [get_ipums_cps()]
#'
#' @inheritParams get_ipums_micro
#' @return a tibble of microdata from the IPUMS API
#' @name ipums_micro_deprecated
NULL

#' @rdname ipums_micro_deprecated
#' @export
dl_ipums_micro = function(extract) {
  lifecycle::deprecate_soft("2.0.0", "dl_ipums_micro()", "get_ipums_micro()")
  get_ipums_micro(extract)
}

#' @rdname ipums_micro_deprecated
#' @export
dl_ipums_acs1 = function(years = NULL, variables, description = NULL, ...) {
  lifecycle::deprecate_soft("2.0.0", "dl_ipums_acs1()", "get_ipums_acs1()")
  get_ipums_acs1(years = years, variables = variables, description = description, ...)
}

#' @rdname ipums_micro_deprecated
#' @export
dl_ipums_asec = function(years = NULL, variables, description = NULL, ...) {
  lifecycle::deprecate_soft("2.0.0", "dl_ipums_asec()", "get_ipums_asec()")
  get_ipums_asec(years = years, variables = variables, description = description, ...)
}

#' @rdname ipums_micro_deprecated
#' @export
dl_ipums_cps = function(months = NULL, variables, description = NULL, ...) {
  lifecycle::deprecate_soft("2.0.0", "dl_ipums_cps()", "get_ipums_cps()")
  get_ipums_cps(months = months, variables = variables, description = description, ...)
}
