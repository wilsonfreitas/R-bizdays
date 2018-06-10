#' Checks if the given dates are business days.
#'
#' Returns \code{TRUE} if the given date is a business day and \code{FALSE}
#' otherwise.
#'
#' @param dates dates to be checked
#' @param cal the calendar's name
#' 
#' @section Date types accepted:
#' 
#' The argument \code{dates} accepts \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#' 
#' @return
#' \code{logical} objects informing that given dates are or are not business days.
#' 
#' @examples
#' create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' is.bizday("2013-01-02", "Brazil/ANBIMA")
#' 
#' # Once you have a default calendar set, cal does not need to be provided
#' bizdays.options$set(default.calendar="Brazil/ANBIMA")
#' 
#' dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
#' is.bizday(dates)
#' 
#' @export
is.bizday <- function(dates, cal) UseMethod("is.bizday")

#' @export
is.bizday.default <- function(dates,
                              cal = bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  is.bizday(dates, cal)
}

#' @export
is.bizday.Date <- function(dates,
                           cal = bizdays.options$get('default.calendar')) {
  cal <- check_calendar(cal)
  if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
    stop('Given date out of range.')
  cal$is.bizday(as.integer(dates))
}
