\name{offset}
\alias{offset}
\title{Offset the date by n business days.}
\usage{
  offset(cal, date, n)
}
\arguments{
  \item{cal}{an instance of Calendar}

  \item{date}{the date to be offset}

  \item{n}{the amount of business days to offset}
}
\description{
  This function returns the given date offset by the given
  amount of n business days.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
offset(cal, '2013-01-02', 5)
}

