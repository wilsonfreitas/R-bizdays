\name{Calendar}
\alias{Calendar}
\title{Creates the calendar based on a list of holidays.}
\usage{
  Calendar(holidays)
}
\arguments{
  \item{holidays}{a vector of Dates which contains the
  holidays}
}
\description{
  Calendar is the main class, it has all attributes
  necessary to execute business days calculations.
}
\examples{
# holidays has iso-formated dates
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
}

