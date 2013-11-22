\docType{data}
\name{bizdays.options}
\alias{bizdays.options}
\title{bizdays' options}
\format{List of 4
 $ get    :function (name, default = FALSE, drop = TRUE)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 411 9 422 3 16 17 411 422
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10ca95f10> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 423 9 430 2 16 9 423 430
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10ca95f10> 
 $ merge  :function (values)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 431 11 431 55 18 62 431 431
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10ca95f10> 
 $ restore:function (target = value)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 432 13 432 56 20 63 432 432
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10ca95f10>}
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
cal <- Calendar(name='Weekdays')
bizdays.options$set(default.calendar=cal)
bizdays.options$get('default.calendar')
bizdays('2013-07-12', '2013-07-22')
}
\keyword{datasets}

