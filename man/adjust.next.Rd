\name{adjust.next}
\alias{adjust.next}
\title{Adjusts the date to the next business day}
\usage{
  adjust.next(cal, dates)
}
\arguments{
  \item{cal}{an instance of Calendar}

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

