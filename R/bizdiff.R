#' Compute the amount of business days between dates
#'
#' Returns the number of business days between dates in a given vector of dates.
#'
#' @param dates a vector containing the dates to be differenced
#' @param cal the calendar's name
#'
#' @return
#' A `numeric` vector of length `n-1` (where `n` is the input vector length),
#' containing the business days computed between pairs of dates.
#'
#' @section Date types accepted:
#'
#' The arguments \code{from} and \code{to} accept \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#'
#' @examples
#' dates <- c("2017-05-10", "2017-05-12", "2017-05-17")
#' bizdiff(dates, "Brazil/ANBIMA")
#' @export
bizdiff <- function(dates, cal) UseMethod("bizdiff")

#' @export
bizdiff.default <- function(dates,
                            cal = bizdays.options$get("default.calendar")) {
  dates <- as.Date(dates)
  bizdiff(dates, cal)
}

#' @export
bizdiff.Date <- function(dates, cal = bizdays.options$get("default.calendar")) {
  cal <- check_calendar(cal)
  if (!any(dates >= cal$start.date & dates <= cal$end.date)) {
    stop("Given date out of range.")
  }
  if (length(dates) <= 1) {
    return(numeric(0))
  }
  bizdays(utils::head(dates, -1), utils::tail(dates, -1), cal)
}