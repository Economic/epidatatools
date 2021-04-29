#' summarize_groups: summarize distinct groups
#'
#' @param x,y data frames
#' @param ... passed to dplyr::full_join()
#' @return a merged data frame
#' @examples library(dplyr)
#' @examples merge_status(band_members, band_instruments, by = "name")
#' @importFrom magrittr %>%
#' @name merge_status

#' @export
#' @rdname merge_status
merge_status <- function (x, y, ...) {
  UseMethod("merge_status")
}

#' @export
#' @rdname merge_status
merge_status.data.frame = function(x, y, ...) {
  x$"__tmp_x" <- 1L
  y$"__tmp_y" <- 1L
  dplyr::full_join(x, y, ...) %>%
    dplyr::mutate(`_merge` = dplyr::case_when(
      !is.na(`__tmp_x`) & !is.na(`__tmp_y`) ~ "both",
      !is.na(`__tmp_x`) & is.na(`__tmp_y`) ~ "left_only",
      is.na(`__tmp_x`) & !is.na(`__tmp_y`) ~ "right_only"
    )) %>%
    dplyr::select(-c("__tmp_x", "__tmp_y"))
}
