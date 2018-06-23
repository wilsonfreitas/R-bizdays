#' Obtaining dates using other dates (or month or year) as reference
#' 
#' Imagine you have one date and want the first or last day of this date's month.
#' For example, you have the date 2018-02-01 and want the last day of its month.
#' You have to check whether or not its year is a leap year, and this sounds a
#' tough task.
#' \code{getdate} helps with returning specific dates according to a reference
#' than can be another date, a month or an year.
#' 
#' @param expr a character string specifying the date to be returned (see Details)
#' @param ref a \code{ref} object (see Details)
#' @param cal the calendar's name
#' 
#' \code{expr} represents the day has to be returned, here it follows a few examples:
#' \itemize{
#'   \item \code{"second day"}
#'   \item \code{"10th bizday"}
#'   \item \code{"3rd wed"}
#'   \item \code{"last bizday"}
#'   \item \code{"first fri"}
#' }
#' 
#' \code{expr} is a character string with two terms: \code{"<position> <day>"}
#' \itemize{
#'   \item positions: \code{first} or \code{1st},
#'   \code{second} or \code{2nd},
#'   \code{third} or \code{3rd}, \code{last} and \code{XXth} (examples
#'   \code{6th} or \code{11th})
#'   \item days: \code{day}, \code{bizday}, or weekdays (\code{sun}, \code{mon},
#'   \code{tue}, \code{wed}, \code{thu}, \code{fri}, \code{sat})
#' }
#' 
#' \code{getdate} returns dates according to a reference that can be a month or
#' an year. This reference is build with the \code{\link{ref}} object.
#' The \code{ref} object specifies a month or an year based on a date or the month
#' and year can be directly specified.
#' 
#' @seealso \code{\link{ref}}
#' 
#' @examples
#' getdate("first day", ref("2018-01-01", ym = "month"), "actual")
#' getdate("10th wed", ref(2018), "actual")
#' getdate("last bizday", ref(2010:2018), "Brazil/ANBIMA")
#' dts <- seq(as.Date("2018-01-01"), as.Date("2018-12-01"), "month")
#' getdate("first bizday", ref(dts, ym = "month"), "Brazil/ANBIMA")
#' @export
getdate <- function(expr, ref, cal = bizdays.options$get("default.calendar")) {
  cal <- check_calendar(cal)
  tok <- strsplit(expr, "\\s+")[[1]]
  if (length(tok) != 2)
    stop("Invalid expr", expr)
  n <- getnth_(tok[1])
  if (tok[2] == "day") {
    date_res <- lapply(seq_len(NROW(ref$year_month)),
                       function(x) getnthday_(n, ref, cal, x))
    as.Date(unlist(date_res), origin = as.Date("1970-01-01"))
  } else if (tok[2] == "bizday") {
    date_res <- lapply(seq_len(NROW(ref$year_month)),
                       function(x) getnthday_(n, ref, cal, x, TRUE))
    as.Date(unlist(date_res), origin = as.Date("1970-01-01"))
  } else if (tok[2] %in% WEEKDAYS) {
    wday <- which(tok[2] == WEEKDAYS)
    date_res <- lapply(seq_len(NROW(ref$year_month)),
                       function(x) getnthweekday_(n, ref, cal, wday, x))
    as.Date(unlist(date_res), origin = as.Date("1970-01-01"))
  } else
    stop("Invalid expr", expr)
}

#' Creates date references to be used in \code{getdate}
#' 
#' Date references are specifically months or years to be used in 
#' \code{getdate}.
#' Months and years can be specified directly or can be base on a given date.
#' \code{getdate} returns a date that is in the reference passed.
#' 
#' @param x a \code{Date} vector, a character vector (specifying dates, months
#' or years) or a numeric vector (specifying years)
#' @param ym a character string with the values \code{month} or \code{year} (see
#' Details)
#' @param ... additional arguments
#' 
#' If a date (\code{character} or \code{Date}) is passed to \code{ref} it has to
#' specified whether the reference is to the month or the year of the given
#' date. This is set in the argument \code{ym} that accepts \code{month} (default) or 
#' \code{year}.
#' 
#' @examples
#' ref(as.Date("2018-01-01"), "month") # refers to 2018-01
#' ref("2018-01-01", "month")          # refers to 2018-01
#' ref("2018-01-01", "year")           # refers to 2018
#' 
#' ref(c("2018-01", "2018-02")) # refers to 2018-01 and 2018-02
#' ref("2018") # refers to 2018
#' ref(2010:2018) # refers to all years from 2010 to 2018
#' 
#' @name ref
#' @export
ref <- function(x, ...) UseMethod("ref")

#' @rdname ref
#' @export
ref.Date <- function(x, ym = c("month", "year"), ...) {
  ym <- match.arg(ym)
  that <- if (ym == "month") {
    list(
      dates = x,
      by_month = TRUE,
      year_month = cbind(year = YEAR(x), month = MONTH(x))
    )
  } else {
    list(
      dates = x,
      by_month = FALSE,
      year_month = cbind(year = YEAR(x))
    )
  }
  structure(that, class = "ref")
}

#' @rdname ref
#' @export
ref.character <- function(x, ...) {
  that <- if (all(grepl("^(\\d{4})-(\\d{2})$", x))) {
    mx <- regmatches(x, regexec("^(\\d{4})-(\\d{2})$", x))
    mx <- do.call(rbind, mx)
    list(
      by_month = TRUE,
      year_month = cbind(year = as.integer(mx[, 2]),
                         month = as.integer(mx[, 3]))
    )
  } else if (all(grepl("^(\\d{4})$", x))) {
    mx <- regmatches(x, regexec("^(\\d{4})$", x))
    mx <- do.call(rbind, mx)
    list(
      by_month = FALSE,
      year_month = cbind(year = as.integer(mx[, 2]))
    )
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x))) {
    do.call(ref.Date, append(list(...), list(x = as.Date(x))))
  } else
    stop("Invalid character ref ", x)
  structure(that, class = "ref")
}

#' @rdname ref
#' @export
ref.numeric <- function(x, ...) {
  that <- list(
    by_month = FALSE,
    year_month = cbind(year = x)
  )
  structure(that, class = "ref")
}

MONTH <- function(x) as.integer(format(x, "%m"))

YEAR <- function(x) as.integer(format(x, "%Y"))

nth2int <- function(x) {
  rx <- regexec("^(\\d+)th$", x)
  mx <- regmatches(x, rx)[[1]]
  if (length(mx) == 0)
    stop("Invalid position nth", x)
  as.integer(mx[[2]])
}

getnth_ <- function(x) {
  switch(
    x,
    first = 1,
    second = 2,
    third = 3,
    last = -1,
    `1st` = 1,
    `2nd` = 2,
    `3rd` = 3,
    nth2int(x)
  )
}

getnthday_ <- function(pos, ref, cal, ref_pos = 1, use_bizday = FALSE) {
  ix <- if (ref$by_month) {
    ix_ <- cal$dates.table[, "month"] == ref$year_month[ref_pos, "month"] &
      cal$dates.table[, "year"] == ref$year_month[ref_pos, "year"]
    if (use_bizday) ix_ & cal$dates.table[, "is_bizday"] == 1 else ix_
  } else {
    ix_ <- cal$dates.table[, "year"] == ref$year_month[ref_pos, "year"]
    if (use_bizday) ix_ & cal$dates.table[, "is_bizday"] == 1 else ix_
  }
  x <- cal$dates.table[ix, ]
  pos <- if (pos < 0) NROW(x) else pos
  res <- as.Date(x[pos, "dates"], origin = as.Date("1970-01-01"))
  unname(res)
}

getnthweekday_ <- function(pos, ref, cal, wday, ref_pos = 1) {
  ix <- if (ref$by_month) {
    ix_ <- cal$dates.table[, "month"] == ref$year_month[ref_pos, "month"] &
      cal$dates.table[, "year"] == ref$year_month[ref_pos, "year"]
    ix_ & cal$dates.table[, "weekday"] == wday
  } else {
    ix_ <- cal$dates.table[, "year"] == ref$year_month[ref_pos, "year"]
    ix_ & cal$dates.table[, "weekday"] == wday
  }
  x <- cal$dates.table[ix, ]
  pos <- if (pos < 0) NROW(x) else pos
  res <- as.Date(x[pos, "dates"], origin = as.Date("1970-01-01"))
  unname(res)
}

WEEKDAYS <- c("thu", "fri", "sat", "sun", "mon", "tue", "wed")
