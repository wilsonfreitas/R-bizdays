\name{adjust.previous}
\alias{adjust.previous}
\title{Adjusts the date to the previous business day}
\usage{
  adjust.previous(cal, dates)
}
\arguments{
  \item{cal}{an instance of Calendar}

  \item{dates}{the date or a vector of dates to be
  adjusted}
}
\description{
  Moves the given date to the previous business day, once
  it is a non-business day.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
adjust.previous(cal, '2013-01-01')
}

