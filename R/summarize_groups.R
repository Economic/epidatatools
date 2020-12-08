#' summarize_groups: summarize distinct groups
#'
#' @param data a data frame
#' @param ... one or two variables, for a one- or two-way cross-tabulation
#' @param w weight
#' @param col column percentages in a two-way cross-tabulation
#' @param row row percentages in a two-way cross-tabulation
#' @return a tibble
#' @importFrom magrittr %>%
#' @examples groups <- c("cyl", "gear", "carb")
#' @examples summarize_groups(mtcars, groups, median(mpg), mean(hp))

#' @export
summarize_groups <- function(.data, .groups, ...) {
  dots <- rlang::enquos(...)

  purrr::map_dfr(.groups, ~ summarize_onegroup(.data, tidyselect::all_of(.x), !!!dots)) %>%
    dplyr::relocate(name, value)
}

summarize_onegroup <- function(.data, .group, ...) {
  data <- .data %>%
    dplyr::rename(value = {{ .group }}) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(..., .groups = "drop") %>%
    dplyr::mutate(name = .group)

  if (haven::is.labelled(data$value)) {
    data %>%
      dplyr::mutate(value_label = as.character(haven::as_factor(value))) %>%
      haven::zap_labels() %>%
      dplyr::relocate(name, value, value_label)
  }
  else {
    data %>% dplyr::relocate(name, value)
  }
}



