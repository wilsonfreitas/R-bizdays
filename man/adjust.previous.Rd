\name{adjust.previous}
\alias{adjust.previous}
\alias{adjust.previous.Calendar}
\alias{adjust.previous.Date}
\alias{adjust.previous.character}
\title{Adjusts the date to the previous business day}
\usage{
  adjust.previous(object, ...)

  \method{adjust.previous}{Calendar} (cal, dates)

  \method{adjust.previous}{character} (dates)

  \method{adjust.previous}{Date} (dates)
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

