\name{Calendar}
\alias{Calendar}
\title{Creates the calendar based on a list of holidays.}
\usage{
  Calendar(holidays = integer(0),
    start.date = "1970-01-01", end.date = "2071-01-01",
    name = NULL, weekdays = c("saturday", "sunday"))
}
\arguments{
  \item{holidays}{a vector of Dates which contains the
  holidays}

  \item{start.date}{the date which calendar starts}

  \item{end.date}{the date which calendar ends}

  \item{name}{calendar's name}
}
\description{
  Calendar is the main class, it has all attributes
  necessary to execute business days calculations.
}
\examples{
# holidays has iso-formated dates
data(holidaysANBIMA)
cal <- Calendar(name='ANBIMA', holidays=holidaysANBIMA)
# For empty calendar just pass nothing
cal <- Calendar(name='Weekdays') # from 1970-01-01 to 2071-01-01
# unnamed calendars have NULL names
cal <- Calendar(start.date='1976-07-12', end.date='2013-10-28')
is.null(name(cal)) # TRUE
}

