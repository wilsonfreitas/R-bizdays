
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
	    return( ! ( wday == 2 || wday == 3 || any(. == n.holidays)) )
	}, logical(1))
	n.bizdays <- n.dates[.is.bizday]
	idx <- as.integer(1)
	index <- vapply(.is.bizday, function(.) {
	    prev.idx <- idx
	    idx <<- prev.idx + as.integer(.)
	    return( prev.idx )
	}, integer(1))
	# class attributes
	bizdays <- dates[.is.bizday]
	that$adjust.next <- function(date) {
	    date <- as.Date(date)
	    while ( ! .is.bizday[dates == date] ) date <- date + 1
	    return( date )
	}
	that$adjust.previous <- function(date) {
	    date <- as.Date(date)
	    while ( ! .is.bizday[dates == date] ) date <- date - 1
	    return( date )
	}
	that$bizdays <- function(from, to) {
		from.idx <- index[dates == that$adjust.next(from)]
		to.idx <- index[dates == that$adjust.previous(to)]
		return( to.idx - from.idx )
	}
	that$is.bizday <- function(date) .is.bizday[dates == date]
	that$seq <- function(from, to) {
		bizdays[which(bizdays >= from & bizdays <= to)]
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
#' @param date the date to be adjusted
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' adjust.next(cal, '2013-01-01')
adjust.next <- function(cal, date) cal$adjust.next(date)

#' Adjusts the date to the previous business day
#'
#' Moves the given date to the previous business day, once it is a
#' non-business day.
#'
#' @param cal an instance of Calendar
#' @param date the date to be adjusted
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' adjust.previous(cal, '2013-01-01')
adjust.previous <- function(cal, date) cal$adjust.previous(date)

#' Computes business days between two dates.
#'
#' This function computes the amount of business days between 2 taking into
#' account the holidays passed to the Calendar function.
#'
#' @param cal an instance of Calendar
#' @param from the initial date
#' @param to the final date. This date must be greater that the initial date
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
#' @param date the date to be tested
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' is.bizday(cal, '2013-01-02')
is.bizday <- function(cal, date) cal$is.bizday(date)

#' @S3method seq Calendar
seq.Calendar <- function(cal, from, to) cal$seq(from, to)

#' Offset the date by n business days.
#'
#' This function returns the given date offset by the given amount of n business
#' days.
#'
#' @param cal an instance of Calendar
#' @param date the date to be offset
#' @param n the amount of business days to offset
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' offset(cal, '2013-01-02', 5)
offset <- function(cal, date, n) {
    if (n >= 0) {
        adjust <- function(date) adjust.next(cal, date)
        date <- adjust(date)
        inc <- 1
    } else {
        adjust <- function(date) adjust.previous(cal, date)
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