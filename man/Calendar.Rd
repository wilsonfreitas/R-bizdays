\name{Calendar}
\alias{Calendar}
\title{Creates the calendar based on a list of holidays.}
\usage{
  Calendar(holidays = integer(0),
    start.date = "1970-01-01", end.date = "2071-01-01")
}
\arguments{
  \item{holidays}{a vector of Dates which contains the
  holidays}

  \item{start.date}{the date which calendar starts}

  \item{end.date}{the date which calendar ends}
}
\description{
  Calendar is the main class, it has all attributes
  necessary to execute business days calculations.
}
\examples{
# holidays has iso-formated dates
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
# For empty calendar just pass nothing
cal <- Calendar()
}

