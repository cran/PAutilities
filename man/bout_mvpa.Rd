% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bout_mvpa.R
\name{bout_mvpa}
\alias{bout_mvpa}
\title{Classify moderate-to-vigorous physical activity in bouts of a specific
minimum length}
\usage{
bout_mvpa(
  intensity,
  var_type = c("METs", "Intensity"),
  min_duration = 10,
  termination = 3,
  MoreArgs = list(breaks = c(-Inf, 1.51, 3, Inf), labels = c("SB", "LPA", "MVPA"), right
    = FALSE),
  ...,
  timestamps = NULL,
  output_var = c("is_MVPA", "bout_tracker")
)
}
\arguments{
\item{intensity}{a vector of intensity classifications to be re-classified
according to the bout definition}

\item{var_type}{character scalar indicating whether the \code{intensity}
variable is a numeric vector of metabolic equivalents, or a factor variable
giving activity intensity classification}

\item{min_duration}{numeric scalar: minimum duration of a qualifying bout, in
minutes}

\item{termination}{numeric scalar: consecutive minutes of non-MVPA required
to terminate the bout}

\item{MoreArgs}{required arguments passed to \code{cut}}

\item{...}{optional arguments passed to \code{cut} for converting METs to
intensity classification}

\item{timestamps}{optional vector of POSIX-formatted timestamps. Must have
same length as \code{intensity}}

\item{output_var}{the output variable(s) to give}
}
\value{
based on the setting of \code{output_var}, one or both of
  \code{is_MVPA} and \code{bout_tracker} will be returned, the former being
  a vector of indicators (1 or 0) specifying whether a minute is part of a
  valid MVPA bout, and the latter being a collapsed data frame giving only
  the valid bouts of MVPA and the relevant information (i.e., duration of
  the bout, minutes of MVPA, and percentage of time spent in MVPA within
  the bout). If both are selected, they are returned in a list.
}
\description{
Classify moderate-to-vigorous physical activity in bouts of a specific
minimum length
}
\examples{
data(ex_data, package = "PAutilities")
ex_data$DateTime <- as.POSIXct(ex_data$DateTime, "UTC")

\donttest{
# Runs with a warning

bout_mvpa(ex_data$METs, "METs")

bout_mvpa(ex_data$METs, "METs", timestamps = ex_data$DateTime)
}

# Recommended usage
lapply(split(ex_data, strftime(ex_data$DateTime, "\%Y-\%m-\%d", "UTC")),
function(x) {
bout_mvpa(x$METs, "METs", timestamps = x$DateTime)
})

lapply(split(ex_data, strftime(ex_data$DateTime, "\%Y-\%m-\%d", "UTC")),
function(x) {
bout_mvpa(x$METs,
"METs",
timestamps = x$DateTime,
output_var = "is_MVPA")
})

lapply(split(ex_data, strftime(ex_data$DateTime, "\%Y-\%m-\%d", "UTC")),
function(x) {
bout_mvpa(x$METs,
"METs",
timestamps = x$DateTime,
output_var = "bout_tracker")
})

}
