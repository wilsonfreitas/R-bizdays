\docType{data}
\name{bizdays.options}
\alias{bizdays.options}
\title{bizdays' options}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 310 9 321 3 16 17 310 321
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x101e49600> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 322 9 329 2 16 9 322 329
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x101e49600> 
 $ merge  :function (values)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 330 11 330 55 18 62 330 330
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x101e49600> 
 $ restore:function (target = value)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 331 13 331 56 20 63 331 331
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x101e49600>}
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

