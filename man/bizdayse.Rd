\name{bizdayse}
\alias{bizdayse}
\alias{bizdayse.Date}
\alias{bizdayse.POSIXct}
\alias{bizdayse.POSIXlt}
\alias{bizdayse.character}
\alias{bizyearse}
\alias{bizyearse.Date}
\alias{bizyearse.POSIXct}
\alias{bizyearse.POSIXlt}
\alias{bizyearse.character}
\title{Business days and current days equivalence}
\usage{
bizdayse(dates, curd, cal)

\method{bizdayse}{character}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

\method{bizdayse}{POSIXct}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

\method{bizdayse}{POSIXlt}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

\method{bizdayse}{Date}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

bizyearse(dates, curd, cal)

\method{bizyearse}{character}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

\method{bizyearse}{POSIXct}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

\method{bizyearse}{POSIXlt}(dates, curd,
  cal = bizdays.options$get("default.calendar"))

\method{bizyearse}{Date}(dates, curd,
  cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{dates}{the initial date (or a vector of dates)}

  \item{curd}{the amount of current days (or a vector of
  numeric)}

  \item{cal}{an instance of Calendar}
}
\description{
\code{bizdayse} stands for business days equivalent, it
returns the amount of business days equivalent to a given
number of current days.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA, dib=252)
bizdayse("2013-01-02", 3, cal)
bizyearse("2013-01-02", 3, cal)
}

