#' summarize_groups: summarize distinct groups
#'
#' @param .data a data frame
#' @param .groups grouping variables
#' @param ... name-value pairs passed to dplyr::summarize()
#' @return a tibble
#' @importFrom magrittr %>%
#' @examples summarize_groups(mtcars, cyl|gear|carb, median(mpg), mean(hp))

#' @export
summarize_groups <- function(.data, .groups, ...) {
  dots <- rlang::enquos(...)

  vars <- .data %>%
    ungroup() %>%
    dplyr::select({{ .groups }}) %>%
    colnames()

  purrr::map_dfr(vars, ~ summarize_onegroup(.data,
                                            tidyselect::all_of(.x),
                                            !!!dots))

}

summarize_onegroup <- function(.data, .group, ...) {

  data <- .data %>%
    dplyr::rename(group_value = {{ .group }}) %>%
    dplyr::group_by(group_value, .add = TRUE) %>%
    dplyr::summarize(...) %>%
    dplyr::mutate(group_name := .group)

  # order variables
  if (haven::is.labelled(data$group_value)) {
    data <- data %>%
      dplyr::mutate(group_value_label = as.character(haven::as_factor(group_value))) %>%
      haven::zap_labels() %>%
      dplyr::relocate(group_name, group_value, group_value_label)
  }
  else {
    data <- data %>% dplyr::relocate(group_name, group_value)
  }

  # promote original group variables to the front
    data <- relocate(data, group_vars(data))

  data
}



