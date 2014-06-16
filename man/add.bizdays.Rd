\name{add.bizdays}
\alias{add.bizdays}
\alias{add.bizdays.Date}
\alias{add.bizdays.POSIXct}
\alias{add.bizdays.POSIXlt}
\alias{add.bizdays.character}
\title{Adds \code{n} business days to the given \code{dates}.}
\usage{
add.bizdays(dates, n, cal)

\method{add.bizdays}{character}(dates, n,
  cal = bizdays.options$get("default.calendar"))

\method{add.bizdays}{POSIXct}(dates, n,
  cal = bizdays.options$get("default.calendar"))

\method{add.bizdays}{POSIXlt}(dates, n,
  cal = bizdays.options$get("default.calendar"))

\method{add.bizdays}{Date}(dates, n,
  cal = bizdays.options$get("default.calendar"))
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
add.bizdays("2013-01-02", 5, cal)
dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
add.bizdays(dates, 1, cal)
}

