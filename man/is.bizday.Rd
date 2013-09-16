\name{is.bizday}
\alias{is.bizday}
\title{Checks if the given date is a business day.}
\usage{
  is.bizday(cal, dates)
}
\arguments{
  \item{cal}{an instance of Calendar}

  \item{dates}{a date or a vector of dates to be tested}
}
\description{
  This function returns TRUE if the given date is a
  business day and FALSE otherwise.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
is.bizday(cal, '2013-01-02')
dates <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
is.bizday(cal, dates)
}

