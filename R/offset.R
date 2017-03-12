#' Offsets the given \code{dates} by \code{n} business days
#'
#' Returns the given \code{dates} offset by the given amount of \code{n}
#' business days.
#' 
#' @param dates dates to be offset
#' @param n the amount of business days to offset
#' @param cal the calendar's name
#' 
#' @details
#' The argument \code{n} accepts a sequence of integers and if its length
#' differs from \code{dates}' length, the recycle rule is applied to fulfill the
#' gap.
#' 
#' @return
#' \code{Date} objects offset by the amount of days defined.
#' 
#' @section Date types accepted:
#' 
#' The argument \code{dates} accepts \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#' 
#' @section Recycle rule:
#' 
#' These arguments handle the recycle rule so a vector of dates and a vector of
#' numbers can be provided and once those vectors differs in length the recycle
#' rule is applied.
#' 
#' @name offset
#' 
#' @examples
#' create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"),
#'                 adjust.from=adjust.next, adjust.to=adjust.previous)
#' offset("2013-01-02", 5, "Brazil/ANBIMA")
#' 
#' # Once you have a default calendar set, cal does not need to be provided
#' bizdays.options$set(default.calendar="Brazil/ANBIMA")
#' 
#' dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
#' is.bizday(dates)
#' offset(dates, 1)
#' 
#' @export
offset <- function(dates, n, cal) UseMethod('add.bizdays')

#' @rdname offset
#' @export
add.bizdays <- function(dates, n, cal) UseMethod('add.bizdays')

#' @export
add.bizdays.default <- function(dates, n, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  add.bizdays(dates, n, cal)
}

#' @export
offset.default <- add.bizdays.default

#' @export
add.bizdays.Date <- function(dates, n, cal=bizdays.options$get('default.calendar')) {
  cal <- check_calendar(cal)
  if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
    stop('Given date out of range.')
  dates <- as.integer(dates)
  n <- if (length(dates) > length(n)) rep_len(n, length(dates)) else n
  dates <- if (length(dates) < length(n)) rep_len(dates, length(n)) else dates
  dates <- cal$add(dates, n)
  dates <- as.Date(dates, origin='1970-01-01')
  dates
}

#' @export
offset.Date <- add.bizdays.Date


