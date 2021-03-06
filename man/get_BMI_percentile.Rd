% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_BMI_percentile.R
\name{get_BMI_percentile}
\alias{get_BMI_percentile}
\title{Calculate youth BMI percentile from CDC standards}
\usage{
get_BMI_percentile(weight_kg, height_cm, age_yrs, age_mos = NULL,
  sex = c("M", "F"), output = c("percentile", "classification", "both"))
}
\arguments{
\item{weight_kg}{Weight in kilograms}

\item{height_cm}{height in centimeters}

\item{age_yrs}{age in years}

\item{age_mos}{age in months (optional)}

\item{sex}{Character scalar indicating participant's sex}

\item{output}{What should be returned: raw percentile, weight status
classification, or both?}
}
\value{
One of: A numeric scalar giving the BMI percentile (for \code{output
  = "percentile"}); a factor scalar giving the weight status (for
  \code{output = "classification"}); or a list with the percentile and
  classification (for \code{output = "both"}).
}
\description{
Calculate youth BMI percentile from CDC standards
}
\details{
If \code{age_mos} is \emph{not} provided, it will be calculated
  based on \code{age_yrs}, assuming 365.2425 days per year and 30.4375 days
  per month. Depending on how the initial age calculation was made, rounding
  error will occur. Thus, use of the \code{\link{get_age}} function is
  recommended. If \code{age_mos} \emph{is} provided, \code{age_yrs} can be
  passed as \code{NULL}.
}
\examples{
get_BMI_percentile(39.4, 144.5, 12.35, sex = "M")
}
\references{
This function was developed with reference to public domain resources
provided by the Centers for Disease Control and Prevention. For more
information, see:

\url{https://www.cdc.gov/obesity/childhood/defining.html}

\url{https://www.cdc.gov/healthyweight/assessing/bmi/childrens_bmi/tool_for_schools.html}
}
