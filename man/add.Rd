\name{add}
\alias{add}
\alias{add.Calendar}
\alias{add.Date}
\alias{add.character}
\title{Adds \code{n} business days to the given \code{dates}.}
\usage{
  add(obj, ...)

  \method{add}{Calendar} (cal, dates, n)

  \method{add}{character} (dates, n)

  \method{add}{Date} (dates, n)
}
\arguments{
  \item{obj}{an instance of Calendar}

  \item{dates}{a date or a vector of dates to be offset}

  \item{n}{the amount of business days to add}
}
\description{
  This function returns the given \code{dates} offset by
  the given amount of \code{n} business days.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
add(cal, '2013-01-02', 5)
dates <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
add(cal, dates, 1)
}

