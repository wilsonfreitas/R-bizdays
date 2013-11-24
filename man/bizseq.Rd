\name{bizseq}
\alias{bizseq}
\alias{bizseq.Date}
\alias{bizseq.character}
\title{Create a sequence of business days.}
\usage{
  bizseq(from, to, cal)

  \method{bizseq}{character} (from, to,
    cal = bizdays.options$get("default.calendar"))

  \method{bizseq}{Date} (from, to,
    cal = bizdays.options$get("default.calendar"))
}
\arguments{
  \item{from}{the initial date}

  \item{to}{the final date. This date must be greater that
  the initial date}

  \item{cal}{an instance of Calendar}
}
\description{
  This function returns a sequence of business days
  according to the given calendar.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
bizseq("2013-01-02", "2013-01-31", cal)
}

