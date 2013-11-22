\name{adjust.next}
\alias{adjust.next}
\alias{adjust.next.Calendar}
\alias{adjust.next.Date}
\alias{adjust.next.character}
\title{Adjusts the date to the next business day}
\usage{
  adjust.next(obj, ...)

  \method{adjust.next}{Calendar} (obj, dates, ...)

  \method{adjust.next}{character} (dates)

  \method{adjust.next}{Date} (dates)
}
\arguments{
  \item{obj}{an instance of Calendar}

  \item{dates}{the date or a vector of dates to be
  adjusted}
}
\description{
  Moves the given date to the next business day, once it is
  a non-business day.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
adjust.next(cal, '2013-01-01')
}

