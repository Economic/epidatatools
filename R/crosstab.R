#' Cross-tabulate one or two variables
#'
#' @param data a data frame
#' @param ... one or two variables, for a one- or two-way cross-tabulation
#' @param w weight
#' @param percent for a two-way cross-tabulation, replace counts with row or column percentages
#'  - NULL, the default, shows counts rather than percentages
#'  - "row" will replace counts with row percentages, summing to 100% across columns within each row
#'  - "column" will replace counts with column percentages, adding to 100% across rows within each column
#' @return a tibble
#' @export
#' @importFrom rlang .data
#' @examples crosstab(mtcars, cyl)
#' @examples crosstab(mtcars, cyl, gear)
#' @examples crosstab(mtcars, cyl, gear, w = mpg, percent = "column")
crosstab <- function(data, ... , w = NULL, percent = NULL) {

  # parse col names
  col_names <- rlang::enquos(...)

  # syntax checks
  if (length(col_names) > 2) {
    stop("Only one-way or two-way crosstabs allowed")
  }

  if (length(col_names) == 0) {
    stop("You need to enter one or two column names")
  }

  if (!is.null(percent)) {
    if(!percent %in% c("row", "column")) stop(paste("percent =", percent, "is not a valid option"))
  }

  # ungroup data
  data <- dplyr::ungroup(data)

  # prep for all crosstabs
  out <- dplyr::count(data, ..., wt = {{ w }})

  # One-way crosstab
  if (length(col_names) == 1) {
    if (!is.null(percent)) {
      warning("Column and row percent options are ignored in a one-way cross-tabulation")
    }

    out <- out |>
      dplyr::mutate(
        percent = .data$n/sum(.data$n),
        cumul_percent = cumsum(.data$percent)) |>
      dplyr::as_tibble()
  }

  # Two-way crosstab
  if (length(col_names) == 2) {

    # col & row percentages
    # col percentages

    if (!is.null(percent)) {
      if (percent == "column") {
        out <- out |>
          dplyr::group_by(!! col_names[[2]]) |>
          dplyr::mutate(n = .data$n / sum(.data$n)) |>
          dplyr::ungroup()
      }

      # row percentages
      if (percent == "row") {
        out <- out |>
          dplyr::group_by(!! col_names[[1]]) |>
          dplyr::mutate(n = .data$n / sum(.data$n)) |>
          dplyr::ungroup()
      }
    }

    out <- out |>
      dplyr::mutate(dplyr::across(!! col_names[[2]], ~ as.character(haven::as_factor(.x)))) |>
      tidyr::pivot_wider(id_cols = !! col_names[[1]], names_from = !! col_names[[2]], values_from = "n") |>
      dplyr::mutate(dplyr::across(-c(1), ~ tidyr::replace_na(.x, 0)))
  }

  out

}


