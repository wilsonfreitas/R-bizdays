\docType{data}
\name{bizdays.options}
\alias{bizdays.options}
\title{bizdays' options}
\format{a \code{list} with \code{get} and \code{set} functions attached
\preformatted{
bizdays.options$set(key=value)
bizdays.options$get("key")
}}
\usage{
bizdays.options
}
\description{
\code{bizdays.options} defines the default calendar to be
used when calling to the functions: \code{bizdays},
\code{adjust.next}, \code{adjust.previous},
\code{is.bizday}, \code{bizseq}, \code{offset}; without
providing a \code{Calendar} instance as a parameter.
}
\examples{
cal <- Calendar(name="Weekdays")
bizdays.options$set(default.calendar=cal)
bizdays.options$get("default.calendar")
bizdays("2013-07-12", "2013-07-22")
}
\keyword{datasets}

