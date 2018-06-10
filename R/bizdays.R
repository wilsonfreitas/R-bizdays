#' Computes business days between two dates.
#'
#' Returns the amount of business days between 2 dates taking into account the
#' provided \code{Calendar} (or \code{bizdays.options$get("default.calendar")}).
#' 
#' @param from the initial dates
#' @param to the final dates
#' @param cal the calendar's name
#' 
#' @section Date types accepted:
#' 
#' The arguments \code{from} and \code{to} accept \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#' 
#' @section Recycle rule:
#' 
#' These arguments handle the recycle rule so vectors of dates can be provided
#' and once those vectors differs in length the recycle rule is applied.
#' 
#' @section Date adjustment:
#' 
#' \code{from} and \code{to} are adjusted when nonworking dates are
#' provided. Since \code{bizdays} function returns the amount of business days
#' between 2 dates, it must start and end in business days. 
#' The default behavior, that is defined in \code{Calendar}'s instantiation with
#' \code{adjust.from} and \code{adjust.to}, reproduces the Excel's NETWORKDAYS.
#' A common and useful setting is \code{adjust.to=adjust.next} which moves
#' expiring maturities to the next business day, once it is not.
#' 
#' @return
#' \code{integer} objects representing the amount of business days.
#' 
#' @examples
#' create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' bizdays("2013-01-02", "2013-01-31", "Brazil/ANBIMA")
#' 
#' # Once you have a default calendar set, cal does not need to be provided
#' bizdays.options$set(default.calendar="Brazil/ANBIMA")
#' bizdays("2013-01-02", "2013-01-31")
#' 
#' dates <- bizseq("2013-01-01", "2013-01-10")
#' bizdays(dates, "2014-01-31")
#' 
#' @export
bizdays <- function(from, to, cal) UseMethod('bizdays')

#' @export
bizdays.default <- function(from, to,
                            cal = bizdays.options$get('default.calendar')) {
  from <- as.Date(from)
  bizdays(from, to, cal)
}

#' @export
bizdays.Date <- function(from, to,
                         cal = bizdays.options$get('default.calendar')) {
  to <- as.Date(to)
  # ---
  if (all(is.na(to))) return( rep(NA, max(length(to), length(from))) )
  cal <- check_calendar(cal)
  if ( ! any(from >= cal$start.date & from <= cal$end.date) )
    stop('Given date out of range.')
  if ( ! any(to >= cal$start.date & to <= cal$end.date) )
    stop('Given date out of range.')
  lengths <- c(length(from), length(to))
  if (max(lengths) %% min(lengths) != 0)
    stop("from's length must be multiple of to's length and vice-versa.")
  if (length(from) > length(to))
    to <- as.Date(rep_len(to, length(from)), origin = "1970-01-01")
  else if (length(from) < length(to))
    from <- as.Date(rep_len(from, length(to)), origin = "1970-01-01")
  idx <- from > to
  idx[is.na(idx)] <- FALSE
  new.from <- from
  new.to <- to
  new.from[idx] <- to[idx]
  new.to[idx] <- from[idx]
  new.from <- cal$adjust.from(new.from, cal)
  new.to <- cal$adjust.to(new.to, cal)
  bdays <- cal$bizdays(as.integer(new.from), as.integer(new.to))
  bdays[idx] <- -bdays[idx]
  adj_vec <- as.integer( !(cal$is.bizday(as.integer(new.from)) |
                             cal$is.bizday(as.integer(new.to))) )
  bdays <- bdays - adj_vec
  if (cal$financial)
    bdays
  else
    bdays + 1
}

#' Business days and current days equivalence
#' 
#' \code{bizdayse} stands for business days equivalent, it returns the amount
#' of business days equivalent to a given number of current days.
#' 
#' @param dates the reference dates
#' @param curd the amount of current days
#' @param cal the calendar's name
#' 
#' @return
#' An \code{integer} representing an amount of business days.
#' 
#' @details
#' Let us suppose I have a reference date \code{dates} and I offset that date
#' by \code{curd} current days. \code{bizdayse} returns the business days
#' between the reference date and the new date offset by \code{curd} current
#' days.
#' 
#' This is equivalent to
#' \preformatted{
#' refdate <- Sys.Date()
#' curd <- 10
#' newdate <- refdate + 10 # offset refdate by 10 days
#' # this is equals to bizdayse(refdate, 10)
#' bizdays(refdate, newdate)
#' }
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
#' @examples
#' create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' bizdayse("2013-01-02", 3, "Brazil/ANBIMA")
#' 
#' @export
bizdayse <- function(dates, curd, cal) UseMethod('bizdayse')

#' @export
bizdayse.default <- function(dates, curd,
                             cal = bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  bizdayse(dates, curd, cal)
}

#' @export
bizdayse.Date <- function(dates, curd,
                          cal = bizdays.options$get('default.calendar')) {
  bizdays(dates, dates + curd, cal)
}
