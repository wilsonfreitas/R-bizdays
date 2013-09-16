\docType{package}
\name{bizdays}
\alias{bizdays}
\alias{bizdays-package}
\title{Tools for business days calculations}
\usage{
  bizdays(cal, from, to)
}
\arguments{
  \item{cal}{an instance of Calendar}

  \item{from}{the initial date (or a vector of dates)}

  \item{to}{the final date (or a vector of dates). All of
  these dates must be greater than the initial dates.}
}
\description{
  In many countries the standard approach to price
  derivatives and fixed income instruments involves the use
  of business days. In Brazil, for example, the great
  majoroty of financial insturments are priced on business
  days counting rules. Given that the use of business days
  is somehow vital to handle some tasks. That's the reason
  why bizdays came up, to make this task easier. It assumes
  only a list of dates representing the holidays, no
  hardcode and no magic rules for holiday guessing.

  This function computes the amount of business days
  between 2 taking into account the holidays passed to the
  Calendar function.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
bizdays(cal, '2013-01-02', '2013-01-31')
}

