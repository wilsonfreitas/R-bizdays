\name{bizyears}
\alias{bizyears}
\alias{bizyears.Date}
\alias{bizyears.POSIXct}
\alias{bizyears.POSIXlt}
\alias{bizyears.character}
\title{Computes the period between two dates in years taking into account business days}
\usage{
bizyears(from, to, cal)

\method{bizyears}{character}(from, to,
  cal = bizdays.options$get("default.calendar"))

\method{bizyears}{POSIXct}(from, to,
  cal = bizdays.options$get("default.calendar"))

\method{bizyears}{POSIXlt}(from, to,
  cal = bizdays.options$get("default.calendar"))

\method{bizyears}{Date}(from, to,
  cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{from}{the initial date (or a vector of dates)}

  \item{to}{the final date (or a vector of dates).}

  \item{cal}{an instance of Calendar}
}
\description{
Computes the period between two dates in years taking into
account business days
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA, dib=252)
bizyears("2013-01-02", "2013-01-31", cal)
}

