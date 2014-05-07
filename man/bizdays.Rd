\docType{package}
\name{bizdays}
\alias{bizdays}
\alias{bizdays-package}
\alias{bizdays.Date}
\alias{bizdays.POSIXct}
\alias{bizdays.POSIXlt}
\alias{bizdays.character}
\title{Tools for business days calculations}
\usage{
bizdays(from, to, cal)

\method{bizdays}{character}(from, to,
  cal = bizdays.options$get("default.calendar"))

\method{bizdays}{POSIXct}(from, to,
  cal = bizdays.options$get("default.calendar"))

\method{bizdays}{POSIXlt}(from, to,
  cal = bizdays.options$get("default.calendar"))

\method{bizdays}{Date}(from, to,
  cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{from}{the initial date (or a vector of dates)}

  \item{to}{the final date (or a vector of dates).}

  \item{cal}{an instance of Calendar}
}
\description{
In many countries the standard approach to price
derivatives and fixed income instruments involves the use
of business days. In Brazil, for example, the great
majoroty of financial insturments are priced on business
days counting rules. Given that the use of business days is
somehow vital to handle some tasks. That's the reason why
bizdays came up, to make this task easier. It assumes only
a list of dates representing the holidays, no hardcode and
no magic rules for holiday guessing.

This function computes the amount of business days between
2 taking into account the holidays passed to the Calendar
function.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
bizdays("2013-01-02", "2013-01-31", cal)
# Once you have a default calendar set
bizdays.options$set(default.calendar=cal)
bizdays("2013-01-02", "2013-01-31")
}

