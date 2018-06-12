
handle_adjust <- function(x) {
  switch (x,
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
save_calendar <- function(cal, con) {
  cal_list <- cal2list(cal)
  writeLines(jsonlite::toJSON(cal_list, auto_unbox = TRUE, pretty = TRUE), con)
}

#' @export
load_calendar <- function(con) {
  x <- jsonlite::fromJSON(con)
  adjust_from <- switch (if (is.null(x$adjust.from)) "none" else x$adjust.from,
                         none = adjust.none,
                         following = adjust.next,
                         preceding = adjust.previous
  )
  adjust_to <- switch (if (is.null(x$adjust.to)) "none" else x$adjust.to,
                       none = adjust.none,
                       following = adjust.next,
                       preceding = adjust.previous
  )
  holidays <- if (is.null(x$holidays) || length(x$holidays) == 0)
    integer()
  else
    as.Date(x$holidays)
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
