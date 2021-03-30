#' summarize_groups: summarize distinct groups
#'
#' @param .data a data frame
#' @param .groups grouping variables
#' @param ... name-value pairs passed to dplyr::summarize()
#' @return a tibble
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @examples summarize_groups(mtcars, cyl|gear|carb, median(mpg), mean(hp))

#' @export
summarize_groups <- function(.data, .groups, ...) {
  dots <- rlang::enquos(...)

  vars <- .data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ .groups }}) %>%
    colnames()

  purrr::map_dfr(vars, ~ summarize_onegroup(.data,
                                            tidyselect::all_of(.x),
                                            !!!dots))

}

summarize_onegroup <- function(.data, .group, ...) {

  data <- .data %>%
    dplyr::rename(group_value = {{ .group }}) %>%
    dplyr::group_by(.data$group_value, .add = TRUE) %>%
    dplyr::summarize(...) %>%
    dplyr::mutate(group_name = .group)

  # order variables
  if (haven::is.labelled(data$group_value)) {
    data <- data %>%
      dplyr::mutate(group_value_label = as.character(haven::as_factor(.data$group_value))) %>%
      haven::zap_labels() %>%
      dplyr::relocate(.data$group_name, .data$group_value, .data$group_value_label)
  }
  else {
    data <- dplyr::relocate(data, .data$group_name, .data$group_value)
  }

  # promote original group variables to the front
    data <- dplyr::relocate(data, dplyr::group_vars(data))

  data
}



