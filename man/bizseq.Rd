\name{bizseq}
\alias{bizseq}
\title{Create a sequence of business days.}
\usage{
  bizseq(cal, from, to)
}
\arguments{
  \item{cal}{an instance of Calendar}

  \item{from}{the initial date}

  \item{to}{the final date. This date must be greater that
  the initial date}
}
\description{
  This function returns a sequence of business days
  according to the given calendar.
}
\examples{
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
bizseq(cal, '2013-01-02', '2013-01-31')
}

