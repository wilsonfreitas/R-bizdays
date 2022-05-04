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
  from <- as.Date(from)
  bizseq(from, to, cal)
}

#' @export
bizseq.Date <- function(from, to,
                        cal = bizdays.options$get("default.calendar")) {
  to <- as.Date(to)
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