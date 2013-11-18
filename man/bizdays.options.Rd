\docType{data}
\name{bizdays.options}
\alias{bizdays.options}
\title{bizdays' options}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 347 9 358 3 16 17 347 358
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10c5b7a78> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 359 9 366 2 16 9 359 366
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10c5b7a78> 
 $ merge  :function (values)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 367 11 367 55 18 62 367 367
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10c5b7a78> 
 $ restore:function (target = value)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 368 13 368 56 20 63 368 368
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10c5b7a78>}
\usage{
  bizdays.options
}
\description{
  \code{bizdays.options} defines the default calendar to be
  used at \code{bizdays.default} calls.
}
\examples{
cal <- Calendar(name='Weekdays')
bizdays.options$set(default.calendar=cal)
bizdays.options$get('default.calendar')
}
\keyword{datasets}

