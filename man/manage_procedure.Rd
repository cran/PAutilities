% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage_procedure.R
\name{manage_procedure}
\alias{manage_procedure}
\alias{get_duration}
\title{Printing and timing utility for managing processes}
\usage{
manage_procedure(part = c("Start", "End"), ..., timer = NULL, verbose = TRUE)

get_duration(timer)
}
\arguments{
\item{part}{character scalar, either \code{Start} or \code{End}.}

\item{...}{character strings to print. Default messages will print if no
arguments are provided.}

\item{timer}{a proc_time object. Required for \code{manage_procedure} only if
using the default message for \code{part = "End"} default message.}

\item{verbose}{logical. Print to console?}
}
\value{
For \code{part = "Start"}, a proc_time object (i.e., a timer passable
  to an eventual \code{part = "End"} command); for \code{part = "End"},
  invisible
}
\description{
Printing and timing utility for managing processes
}
\examples{

manage_procedure("Start", "String will be printed\n")
timer <- manage_procedure(
"Start", "Printing a string is optional", verbose = FALSE
)

## Default starting message
manage_procedure("Start")

## Default ending message
manage_procedure("End", timer = timer)

## Other examples
get_duration(timer)
manage_procedure("End", "Custom ending message")

}
