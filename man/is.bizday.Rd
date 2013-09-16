\name{is.bizday}
\alias{is.bizday}
\title{Checks if the given date is a business day.}
\usage{
  is.bizday(cal, date)
}
\arguments{
  \item{cal}{an instance of Calendar}

  \item{date}{the date to be tested}
}
\description{
  This function returns TRUE if the given date is a
  business day and FALSE otherwise.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
is.bizday(cal, '2013-01-02')
}

