\docType{data}
\name{bizdays.options}
\alias{bizdays.options}
\title{bizdays' options}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 303 9 314 3 16 17 303 314
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x1109f6318> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 315 9 322 2 16 9 315 322
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x1109f6318> 
 $ merge  :function (values)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 323 11 323 55 18 62 323 323
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x1109f6318> 
 $ restore:function (target = value)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 324 13 324 56 20 63 324 324
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x1109f6318>}
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

