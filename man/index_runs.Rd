% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_runs.R
\name{index_runs}
\alias{index_runs}
\title{Run length encoding with indices}
\usage{
index_runs(x, zero_index = FALSE)
}
\arguments{
\item{x}{vector of values on which to perform run length encoding}

\item{zero_index}{logical. Should indices be indexed from zero (useful for
Rcpp)?}
}
\value{
A data frame with information about the runs and start/stop indices
}
\description{
Run length encoding with indices
}
\examples{

x <- c(
  FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
  FALSE, TRUE, TRUE, FALSE, TRUE, FALSE,
  FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
  FALSE, TRUE
)
head(index_runs(x))
}
