#' Create a sequence of business days
#'
#' Returns a sequence of dates with business days only.
#'
#' @param from the initial date
#' @param to the final date (must be greater than \code{from})
#' @param cal the calendar's name
#'
#' @return
#' A vector of \code{Date} objects that are business days according to the
#' provided \code{Calendar}.
#'
#' @section Date types accepted:
#'
#' The arguments \code{from} and \code{to} accept \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#'
#' @examples
#' bizseq("2013-01-02", "2013-01-31", "Brazil/ANBIMA")
#' @export
bizseq <- function(from, to, cal) UseMethod("bizseq")

#' @export
bizseq.default <- function(from, to,
                           cal = bizdays.options$get("default.calendar")) {
  from <- to_date(from)
  bizseq(from, to, cal)
}

#' @export
bizseq.Date <- function(from, to,
                        cal = bizdays.options$get("default.calendar")) {
  to <- to_date(to)
  cal <- check_calendar(cal)
  if (!any(from >= cal$start.date & from <= cal$end.date)) {
    stop("Given date out of range.")
  }
  if (!any(to >= cal$start.date & to <= cal$end.date)) {
    stop("Given date out of range.")
  }
  if (!all(from <= to)) {
    stop("All from dates must be greater than all to dates.")
  }
  from <- as.integer(from)
  to <- as.integer(to)
  as.Date(cal$seq(from, to), origin = "1970-01-01")
}

#' Find missing business days in a sequence of dates
#'
#' Returns the business days between \code{from} and \code{to} that are not
#' in \code{dates}. Useful to check if a time series has gaps.
#'
#' @param dates a vector of dates
#' @param cal the calendar's name
#' @param from the initial date, defaults to the first date in \code{dates}
#' @param to the final date, defaults to the last date in \code{dates}
#'
#' @return
#' A vector of \code{Date} objects with the missing business days. It is empty
#' if no business day is missing or if \code{dates} has no valid dates.
#'
#' To find the opposite, dates in \code{dates} that are not business days, use
#' \code{dates[!is.bizday(dates, cal)]}.
#'
#' @examples
#' dates <- bizseq("2013-01-02", "2013-01-31", "Brazil/ANBIMA")[-c(3, 10)]
#' missing_bizdays(dates, "Brazil/ANBIMA")
#' # also check after the last date
#' missing_bizdays(dates, "Brazil/ANBIMA", to = "2013-02-08")
#' @export
missing_bizdays <- function(dates, cal = bizdays.options$get("default.calendar"),
                            from = NULL, to = NULL) {
  dates <- to_date(dates)
  if (is.null(from)) from <- suppressWarnings(min(dates, na.rm = TRUE))
  if (is.null(to)) to <- suppressWarnings(max(dates, na.rm = TRUE))
  from <- to_date(from)
  to <- to_date(to)
  if (!is.finite(from) || !is.finite(to)) {
    return(as.Date(character(0)))
  }
  bs <- bizseq(from, to, cal)
  bs[!bs %in% dates]
}
