% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_status.R
\name{merge_status}
\alias{merge_status}
\alias{merge_status.data.frame}
\title{Join data frames and create a merge indicator}
\usage{
merge_status(x, y, ...)

\method{merge_status}{data.frame}(x, y, ...)
}
\arguments{
\item{x, y}{data frames}

\item{...}{passed to dplyr::full_join()}
}
\value{
a merged data frame from full_join with an extra column \verb{_merge}
}
\description{
Join data frames and create a merge indicator
}
\examples{
library(dplyr)
merge_status(band_members, band_instruments, by = "name")
}
