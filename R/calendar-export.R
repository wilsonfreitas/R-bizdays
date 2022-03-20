
#' @title Import and export calendars
#'
#' @description
#' The calendars can be specified in JSON files and these functions helps with
#' importing and exporting calendars to text files.
#'
#' @param cal the calendar's name
#' @param con a connection object or a character string.
#'
#' @details
#' \code{save_calendar} exports a calendar to a JSON file and
#' \code{load_calendar} imports.
#'
#' In \code{load_calenadar}, the \code{con} argument can be a connection object
#' or a character string specifying either the file or the JSON text.
#'
#' @section JSON calendar's specification:
#'
#' Here's an example of a calendar's specification.
#'
#' \preformatted{
#'   {
#'    "name": "Brazil/ANBIMA",
#'    "weekdays": ["saturday", "sunday"],
#'    "holidays": ["2001-01-01", "2001-02-26", "2001-02-27", "2001-04-13"],
#'    "adjust.from": "following",
#'    "adjust.to": "preceding"
#'    "financial": true,
#'   }
#' }
#'
#' @name calendar-import-export
NULL

handle_adjust <- function(x) {
  switch(x,
    adjust.next = "following",
    adjust.previous = "preceding",
    adjust.none = NULL,
    none = NULL,
    x # preceding or following
  )
}

cal2list <- function(cal) {
  cal_list <- list(
    name = cal$name,
    weekdays = cal$weekdays,
    holidays = cal$holidays,
    financial = cal$financial,
    adjust.from = handle_adjust(cal$adjust.from_label),
    adjust.to = handle_adjust(cal$adjust.to_label)
  )
  cal_list <- Filter(length, cal_list)
  Filter(Negate(is.null), cal_list)
}

#' @export
#' @rdname calendar-import-export
#' @examples
#' con <- tempfile(fileext = ".json")
#' save_calendar("actual", con)
save_calendar <- function(cal, con) {
  cal <- check_calendar(cal)
  cal_list <- cal2list(cal)
  writeLines(jsonlite::toJSON(cal_list, auto_unbox = TRUE, pretty = TRUE), con)
}

#' @export
#' @rdname calendar-import-export
#' @examples
#' load_calendar(con)
load_calendar <- function(con) {
  x <- jsonlite::fromJSON(con)
  adjust_from <- switch(if (is.null(x$adjust.from)) "none" else x$adjust.from,
    none = adjust.none,
    following = adjust.next,
    preceding = adjust.previous
  )
  adjust_to <- switch(if (is.null(x$adjust.to)) "none" else x$adjust.to,
    none = adjust.none,
    following = adjust.next,
    preceding = adjust.previous
  )
  holidays <- if (is.null(x$holidays) || length(x$holidays) == 0) {
    integer()
  } else {
    as.Date(x$holidays)
  }
  cal <- create.calendar(
    x$name,
    holidays,
    weekdays = x$weekdays, # nullable
    start.date = x$start.date, # nullable
    end.date = x$end.date, # nullable
    adjust.from = adjust_from,
    adjust.to = adjust_to,
    financial = if (is.null(x$financial)) FALSE else x$financial
  )
  invisible(cal)
}