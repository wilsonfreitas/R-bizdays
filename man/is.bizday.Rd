\name{is.bizday}
\alias{is.bizday}
\alias{is.bizday.Date}
\alias{is.bizday.POSIXct}
\alias{is.bizday.POSIXlt}
\alias{is.bizday.character}
\title{Checks if the given date is a business day.}
\usage{
is.bizday(dates, cal)

\method{is.bizday}{character}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{is.bizday}{POSIXct}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{is.bizday}{POSIXlt}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{is.bizday}{Date}(dates, cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{dates}{a date or a vector of dates to be checked}

  \item{cal}{an instance of Calendar}
}
\description{
This function returns TRUE if the given date is a business
day and FALSE otherwise.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
is.bizday("2013-01-02", cal)
# Using the default Calendar
dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
is.bizday(dates)
}

