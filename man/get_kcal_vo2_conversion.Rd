% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_kcal_vo2_conversion.R
\name{get_kcal_vo2_conversion}
\alias{get_kcal_vo2_conversion}
\title{Retrieve conversion factors from kilocalories to oxygen consumption}
\usage{
get_kcal_vo2_conversion(RER, kcal_table = c("Lusk", "Peronnet", "both"))
}
\arguments{
\item{RER}{numeric. The respiratory exchange ratio}

\item{kcal_table}{The table to reference for converting kilocalories to
oxygen consumption. See \code{\link{get_kcal_vo2_conversion}}}
}
\value{
numeric vector giving the conversion factor from the specified
  table(s)
}
\description{
Retrieve conversion factors from kilocalories to oxygen consumption
}
\details{
RER values are matched to the table entries based on the minimum
  absolute difference. If there is a tie, the lower RER is taken.
}
\examples{
get_kcal_vo2_conversion(0.85, "both")

}
\references{
Peronnet, F., & Massicotte, D. (1991). Table of nonprotein respiratory
quotient: an update. \emph{Can J Sport Sci}, 16(1), 23-29.

Lusk, G. (1924). Analysis of the oxidation of mixtures of carbohydrate and
fat: a correction. \emph{Journal of Biological Chemistry}, 59, 41-42.
}
