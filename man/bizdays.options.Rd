\docType{data}
\name{bizdays.options}
\alias{bizdays.options}
\title{bizdays' options}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 401 9 412 3 16 17 401 412
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10fe00838> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 413 9 420 2 16 9 413 420
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10fe00838> 
 $ merge  :function (values)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 421 11 421 55 18 62 421 421
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10fe00838> 
 $ restore:function (target = value)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 422 13 422 56 20 63 422 422
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10fe00838>}
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

