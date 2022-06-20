#' Obtaining dates using other dates (or month or year) as reference
#'
#' Imagine you have one date and want the first or last day of this date's
#' month.
#' For example, you have the date 2018-02-01 and want the last day of its month.
#' You have to check whether or not its year is a leap year, and this sounds a
#' tough task.
#' \code{getdate} helps with returning specific dates according to a reference
#' than can be another date, a month or an year.
#'
#' @param expr a character string specifying the date to be returned
#'             (see Details)
#' @param ref a reference which represents a month or year, where the
#'            date has to be found.
#' @param cal the calendar's name
#'
#' \code{expr} represents the day has to be returned, here it follows a few
#' examples:
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
#' an year. This reference can be passed as a character vector representing
#' months or years, or as a numeric vector representing years.
#' The ISO format must be used to represent years or months with character
#' vectors.
#'
#' @examples
#' getdate("10th wed", 2018, "Brazil/ANBIMA")
#' getdate("last bizday", 2010:2018, "Brazil/ANBIMA")
#' dts <- seq(as.Date("2018-01-01"), as.Date("2018-12-01"), "month")
#' getdate("first bizday", format(dts, "%Y-%m"), "Brazil/ANBIMA")
#' @export
getdate <- function(expr, ref, cal = bizdays.options$get("default.calendar")) {
  cal <- check_calendar(cal)
  ref <- ref(ref)
  tok <- strsplit(expr, "\\s+")[[1]]
  if (length(tok) != 2) {
    stop("Invalid expr", expr)
  }
  n <- getnth_(tok[1])
  if (tok[2] == "day") {
    getnthday(ref, n, cal$dates.table, FALSE)
  } else if (tok[2] == "bizday") {
    getnthday(ref, n, cal$dates.table, TRUE)
  } else if (tok[2] %in% WEEKDAYS) {
    wday <- which(tok[2] == WEEKDAYS)
    getnthweekday(ref, n, cal$dates.table, wday)
  } else {
    stop("Invalid expr", expr)
  }
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
#' date. This is set in the argument \code{ym} that accepts \code{month}
#' (default) or \code{year}.
#'
#' @examples
#' ref(as.Date("2018-01-01"), "month") # refers to 2018-01
#' ref("2018-01-01", "month") # refers to 2018-01
#' ref("2018-01-01", "year") # refers to 2018
#'
#' ref(c("2018-01", "2018-02")) # refers to 2018-01 and 2018-02
#' ref("2018") # refers to 2018
#' ref(2010:2018) # refers to all years from 2010 to 2018
#' @noRd
ref <- function(x, ...) UseMethod("ref")

ref.Date <- function(x, ym = c("month", "year"), ...) {
  ym <- match.arg(ym)
  if (ym == "month") {
    ref_by_month(year = YEAR(x), month = MONTH(x))
  } else {
    ref_by_year(year = YEAR(x))
  }
}

ref.character <- function(x, ...) {
  if (all(grepl("^(\\d{4})-(\\d{2})$", x))) {
    mx <- regmatches(x, regexec("^(\\d{4})-(\\d{2})$", x))
    mx <- do.call(rbind, mx)
    ref_by_month(as.integer(mx[, 2]), as.integer(mx[, 3]))
  } else if (all(grepl("^(\\d{4})$", x))) {
    mx <- regmatches(x, regexec("^(\\d{4})$", x))
    mx <- do.call(rbind, mx)
    ref_by_year(as.integer(mx[, 2]))
  } else if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x))) {
    do.call(ref.Date, append(list(...), list(x = as.Date(x))))
  } else {
    stop("Invalid character ref ", x)
  }
}

ref.numeric <- function(x, ...) {
  that <- list(
    by_month = FALSE,
    year_month = cbind(year = x)
  )
  structure(that, class = c("ref", "by_year"))
}

ref_by_year <- function(year) {
  that <- list(
    by_month = FALSE,
    year_month = cbind(year = year)
  )
  structure(that, class = c("ref", "by_year"))
}

ref_by_month <- function(year, month) {
  that <- list(
    by_month = TRUE,
    year_month = cbind(
      year = year,
      month = month
    )
  )
  structure(that, class = c("ref", "by_month"))
}

MONTH <- function(x) as.integer(format(x, "%m"))

YEAR <- function(x) as.integer(format(x, "%Y"))

nth2int <- function(x) {
  rx <- regexec("^(\\d+)th$", x)
  mx <- regmatches(x, rx)[[1]]
  if (length(mx) == 0) {
    stop("Invalid position nth", x)
  }
  as.integer(mx[[2]])
}

getnth_ <- function(x) {
  switch(x,
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

getnthday <- function(ref, ...) {
  UseMethod("getnthday")
}

getnthday.by_month <- function(ref, pos, cal_table, use_bizday = FALSE) {
  ym_table <- unique(ref$year_month)

  date_res <- lapply(
    seq_len(NROW(ym_table)),
    function(x) {
      month <- ym_table[x, "month"]
      year <- ym_table[x, "year"]
      ix <- cal_table[, "month"] == month & cal_table[, "year"] == year
      ix <- if (use_bizday) ix & cal_table[, "is_bizday"] == 1 else ix

      sel_range <- cal_table[ix, ]
      # pos < 0 == last --> NROW(selected_range)
      pos <- if (pos < 0) NROW(sel_range) else pos
      date <- unname(sel_range[pos, "dates"])

      idx <- ref$year_month[, "year"] == year & ref$year_month[, "month"] == month
      list(date = date, index = idx)
    }
  )

  dates <- integer(NROW(ref$year_month))
  for (res in date_res) {
    dates[res$index] <- res$date
  }

  as.Date(dates, origin = as.Date("1970-01-01"))
}

getnthday.by_year <- function(ref, pos, cal_table, use_bizday = FALSE) {
  ym_table <- unique(ref$year_month)

  date_res <- lapply(
    seq_len(NROW(ym_table)),
    function(x) {
      year <- ym_table[x, "year"]
      ix <- cal_table[, "year"] == year
      ix <- if (use_bizday) ix & cal_table[, "is_bizday"] == 1 else ix

      sel_range <- cal_table[ix, ]
      # pos < 0 == last --> NROW(selected_range)
      pos <- if (pos < 0) NROW(sel_range) else pos
      date <- unname(sel_range[pos, "dates"])

      idx <- ref$year_month[, "year"] == year
      list(date = date, index = idx)
    }
  )

  dates <- integer(NROW(ref$year_month))
  for (res in date_res) {
    dates[res$index] <- res$date
  }

  as.Date(dates, origin = as.Date("1970-01-01"))
}

getnthweekday <- function(ref, ...) {
  UseMethod("getnthweekday")
}

getnthweekday.by_month <- function(ref, pos, cal_table, wday) {
  ym_table <- unique(ref$year_month)

  date_res <- lapply(
    seq_len(NROW(ym_table)),
    function(x) {
      month <- ym_table[x, "month"]
      year <- ym_table[x, "year"]
      ix <- cal_table[, "month"] == month & cal_table[, "year"] == year
      ix <- ix & cal_table[, "weekday"] == wday

      sel_range <- cal_table[ix, ]
      # pos < 0 == last --> NROW(selected_range)
      pos <- if (pos < 0) NROW(sel_range) else pos
      date <- unname(sel_range[pos, "dates"])

      idx <- ref$year_month[, "year"] == year & ref$year_month[, "month"] == month
      list(date = date, index = idx)
    }
  )

  dates <- integer(NROW(ref$year_month))
  for (res in date_res) {
    dates[res$index] <- res$date
  }

  as.Date(dates, origin = as.Date("1970-01-01"))
}

getnthweekday.by_year <- function(ref, pos, cal_table, wday) {
  ym_table <- unique(ref$year_month)

  date_res <- lapply(
    seq_len(NROW(ym_table)),
    function(x) {
      year <- ym_table[x, "year"]
      ix <- cal_table[, "year"] == year
      ix <- ix & cal_table[, "weekday"] == wday

      sel_range <- cal_table[ix, ]
      # pos < 0 == last --> NROW(selected_range)
      pos <- if (pos < 0) NROW(sel_range) else pos
      date <- unname(sel_range[pos, "dates"])

      idx <- ref$year_month[, "year"] == year
      list(date = date, index = idx)
    }
  )

  dates <- integer(NROW(ref$year_month))
  for (res in date_res) {
    dates[res$index] <- res$date
  }

  as.Date(dates, origin = as.Date("1970-01-01"))
}

WEEKDAYS <- c("thu", "fri", "sat", "sun", "mon", "tue", "wed")