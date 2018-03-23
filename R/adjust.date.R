#' Adjusts the given dates to the next/previous business day
#'
#' Rolls the given date to the next or previous business day, unless it is a business day.
#'
#' @param dates dates to be adjusted
#' @param cal an instance of \code{Calendar}
#' 
#' @details 
#' 
#' \code{adjust.next} and \code{following} return the next business day if the given date
#' is not a business day.
#' \code{adjust.previous} and \code{preceding} are similar, but return the previous 
#' business day.
#' \code{modified.following} rolls the given date to the next business day, unless 
#' it happens in the next month, in this case it returns the previous business day.
#' \code{modified.preceding} is similar to \code{modified.following}, but rolls the given 
#' date to the previous business day.
#' 
#' @section Date types accepted:
#' 
#' The argument \code{dates} accepts \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#' 
#' @return
#' \code{Date} objects adjusted accordingly.
#' 
#' @name adjust.date
NULL

#' @rdname adjust.date
#' @export
#' @examples
#' cal <- create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' adjust.next("2013-01-01", "Brazil/ANBIMA")
adjust.next <- function(dates, cal) UseMethod("adjust.next")

#' @rdname adjust.date
#' @export
#' @examples
#' following("2013-01-01", cal)
following <- function(dates, cal) UseMethod("following")

#' @export
adjust.next.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  adjust.next(dates, cal)
}

#' @export
adjust.next.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
  cal <- check_calendar(cal)
  if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
    stop('Given date out of range.')
  dates <- as.integer(dates)
  as.Date(cal$adjust.next(dates), origin='1970-01-01')
}

#' @rdname adjust.date
#' @export
adjust.none <- function(dates, cal) dates

#' @export
following.default <- adjust.next.default

#' @export
following.Date <- adjust.next.Date


#' @rdname adjust.date
#' @export
#' @examples
#' modified.following("2016-01-31", cal)
modified.following <- function(dates, cal) UseMethod("modified.following")

#' @export
modified.following.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  modified.following(dates, cal)
}

#' @export
modified.following.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
  cal <- check_calendar(cal)
  if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
    stop('Given date out of range.')
  dates <- as.integer(dates)
  modified(dates, cal$adjust.next, cal$adjust.previous)
}


#' @rdname adjust.date
#' @export
#' @examples
#' adjust.previous("2013-01-01", cal)
adjust.previous <- function(dates, cal) UseMethod("adjust.previous")

#' @rdname adjust.date
#' @export
#' @examples
#' preceding("2013-01-01", cal)
preceding <- function(dates, cal) UseMethod("preceding")

#' @export
adjust.previous.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  adjust.previous(dates, cal)
}

#' @export
adjust.previous.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
  cal <- check_calendar(cal)
  if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
    stop('Given date out of range.')
  dates <- as.integer(dates)
  as.Date(cal$adjust.previous(dates), origin='1970-01-01')
}

#' @export
preceding.default <- adjust.previous.default

#' @export
preceding.Date <- adjust.previous.Date


#' @rdname adjust.date
#' @export
#' @examples
#' modified.preceding("2016-01-01", cal)
modified.preceding <- function(dates, cal) UseMethod("modified.preceding")

#' @export
modified.preceding.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  modified.preceding(dates, cal)
}

#' @export
modified.preceding.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
  cal <- check_calendar(cal)
  if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
    stop('Given date out of range.')
  dates <- as.integer(dates)
  modified(dates, cal$adjust.previous, cal$adjust.next)
}

modified <- function(dates, move1, move2) {
  dtx <- as.Date(move1(dates), origin='1970-01-01')
  idx <- format(dtx, '%m') != format(as.Date(dates, origin='1970-01-01'), '%m')
  dtx[idx] <- as.Date(move2(dates[idx]), origin='1970-01-01')
  dtx
}

