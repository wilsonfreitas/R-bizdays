\name{adjust.next}
\alias{adjust.next}
\alias{adjust.next.Date}
\alias{adjust.next.POSIXct}
\alias{adjust.next.POSIXlt}
\alias{adjust.next.character}
\alias{adjust.previous}
\alias{adjust.previous.Date}
\alias{adjust.previous.POSIXct}
\alias{adjust.previous.POSIXlt}
\alias{adjust.previous.character}
\title{Adjusts the date to the next business day}
\usage{
adjust.next(dates, cal)

\method{adjust.next}{character}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{adjust.next}{Date}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{adjust.next}{POSIXct}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{adjust.next}{POSIXlt}(dates,
  cal = bizdays.options$get("default.calendar"))

adjust.previous(dates, cal)

\method{adjust.previous}{character}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{adjust.previous}{Date}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{adjust.previous}{POSIXct}(dates,
  cal = bizdays.options$get("default.calendar"))

\method{adjust.previous}{POSIXlt}(dates,
  cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{dates}{a vector of dates to be adjusted}

  \item{cal}{an instance of Calendar}
}
\description{
Moves the given date to the next business day, once it is a
non-business day.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
adjust.next("2013-01-01", cal)
adjust.previous("2013-01-01", cal)
}

