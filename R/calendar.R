
#' Creates the calendar based on a list of holidays.
#' 
#' Calendar is the main class, it has all attributes necessary to execute
#' business days calculations.
#'
#' @param holidays a vector of Dates which contains the holidays
#' @export
#' @examples
#' # holidays has iso-formated dates
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
Calendar <- function (holidays) {
	that <- list()
	dates <- seq(from=min(holidays), to=max(holidays), by='day')
	n.dates <- as.integer(dates)
	n.holidays <- as.integer(holidays)
	.is.bizday <- vapply(n.dates, function(.) {
	    wday <- .%%7
	    ! ( wday == 2 || wday == 3 || any(. == n.holidays))
	}, logical(1))
	n.bizdays <- n.dates[.is.bizday]
	idx <- as.integer(1)
	index <- vapply(.is.bizday, function(.) {
	    prev.idx <- idx
	    idx <<- prev.idx + as.integer(.)
	    prev.idx
	}, integer(1))
	# class attributes
	bizdays <- dates[.is.bizday]
	.adjust.next <- function(date) {
	    date <- as.Date(date)
	    while ( ! .is.bizday[dates == date] ) date <- date + 1
	    date
	}
	.adjust.previous <- function(date) {
	    date <- as.Date(date)
	    while ( ! .is.bizday[dates == date] ) date <- date - 1
	    date
	}
    .adjust <- function(dates, .adjust.FUN) {
        o.dates <- integer(length(dates))
        for (i in seq_along(dates)) {
            o.dates[i] <- .adjust.FUN(dates[i])
        }
        as.Date(o.dates, origin='1970-01-01')
    }
    .bizdays <- function(from, to) {
        from.idx <- index[dates %in% that$adjust.next(from)]
        to.idx <- index[dates %in% that$adjust.previous(to)]
        stopifnot(length(from.idx) == length(to.idx))
        to.idx - from.idx
    }
    that$adjust.next <- function(dates) {
        .adjust(dates, .adjust.next)
    }
    that$adjust.previous <- function(dates) {
        .adjust(dates, .adjust.previous)
    }
	that$bizdays <- function(from, to) {
        stopifnot(all(from <= to))
        stopifnot(length(from) == length(to))
        bd <- integer(length(from))
        for (i in seq_along(from)) {
            bd[i] <- .bizdays(from[i], to[i])
        }
        bd
	}
	that$is.bizday <- function(date) .is.bizday[dates %in% date]
	that$seq <- function(from, to) {
		bizdays[which(bizdays >= from & bizdays <= to)]
	}
    that$offset <- function(date, n) {
        if (n >= 0) {
            adjust <- function(date) .adjust(date, .adjust.next)
            date <- adjust(date)
            inc <- 1
        } else {
            adjust <- function(date) .adjust(date, .adjust.previous)
            date <- adjust(date)
            inc <- -1
            n <- abs(n)
        }
        i <- 0
        while (i < n) {
            date <- date + inc
            date <- adjust(date)
            i <- i + 1
        }
        date
    }
	class(that) <- 'Calendar'
	return(that)
}

# is.Calendar <- function(cal) class(cal) == 'Calendar'
# adjust.next <- function(object, ...) UseMethod("adjust.next", object)
# adjust.previous <- function(object, ...) UseMethod("adjust.previous", object)
# bizdays <- function(object, ...) UseMethod("bizdays", object)
# is.bizday <- function(object, ...) UseMethod("is.bizday", object)
# offset <- function(object, ...) UseMethod("offset", object)

#' Adjusts the date to the next business day
#'
#' Moves the given date to the next business day, once it is a
#' non-business day.
#'
#' @param cal an instance of Calendar
#' @param dates the date or a vector of dates to be adjusted
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' adjust.next(cal, '2013-01-01')
adjust.next <- function(cal, dates) cal$adjust.next(dates)

#' Adjusts the date to the previous business day
#'
#' Moves the given date to the previous business day, once it is a
#' non-business day.
#'
#' @param cal an instance of Calendar
#' @param dates the date or a vector of dates to be adjusted
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' adjust.previous(cal, '2013-01-01')
adjust.previous <- function(cal, dates) cal$adjust.previous(dates)

#' Computes business days between two dates.
#'
#' This function computes the amount of business days between 2 taking into
#' account the holidays passed to the Calendar function.
#'
#' @param cal an instance of Calendar
#' @param from the initial date (or a vector of dates) @param to the final date
#' (or a vector of dates). All of these dates must be greater than the initial
#' dates.
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' bizdays(cal, '2013-01-02', '2013-01-31')
bizdays <- function(cal, from, to) cal$bizdays(from, to)

#' Checks if the given date is a business day.
#'
#' This function returns TRUE if the given date is a business day and FALSE
#' otherwise.
#'
#' @param cal an instance of Calendar
#' @param dates a date or a vector of dates to be tested
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' is.bizday(cal, '2013-01-02')
#' dates <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
#' is.bizday(cal, dates)
is.bizday <- function(cal, dates) cal$is.bizday(dates)

#' Create a sequence of business days.
#'
#' This function returns a sequence of business days according to the given
#' calendar.
#'
#' @param cal an instance of Calendar
#' @param from the initial date
#' @param to the final date. This date must be greater that the initial date
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' bizseq(cal, '2013-01-02', '2013-01-31')
bizseq <- function(cal, from, to) cal$seq(from, to)

#' Offset the date by n business days.
#'
#' This function returns the given date offset by the given amount of n business
#' days.
#'
#' @param cal an instance of Calendar
#' @param dates a date or a vector of dates to be offset
#' @param n the amount of business days to offset
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' offset(cal, '2013-01-02', 5)
#' dates <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
#' offset(cal, dates, 1)
offset <- function(cal, dates, n) cal$offset(dates, n)

#' ANBIMA's holidays list
#' 
#' A dataset containing a list of holidays delivered by ANBIMA
#' (www.anbima.com.br).
#' 
#' @docType data
#' @keywords datasets
#' @format a vector with Date objects
#' @name holidaysANBIMA
NULL