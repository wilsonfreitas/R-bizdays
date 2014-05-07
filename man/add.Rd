\name{add}
\alias{add}
\alias{add.Date}
\alias{add.POSIXct}
\alias{add.POSIXlt}
\alias{add.character}
\title{Adds \code{n} business days to the given \code{dates}.}
\usage{
add(dates, n, cal)

\method{add}{character}(dates, n,
  cal = bizdays.options$get("default.calendar"))

\method{add}{POSIXct}(dates, n, cal = bizdays.options$get("default.calendar"))

\method{add}{POSIXlt}(dates, n, cal = bizdays.options$get("default.calendar"))

\method{add}{Date}(dates, n, cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{dates}{a date or a vector of dates to be offset}

  \item{n}{the amount of business days to add}

  \item{cal}{an instance of Calendar}
}
\description{
This function returns the given \code{dates} offset by the
given amount of \code{n} business days.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
add("2013-01-02", 5, cal)
dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
add(dates, 1, cal)
}

