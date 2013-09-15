

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

adjust.next <- function(object, ...) UseMethod("adjust.next", object)
adjust.previous <- function(object, ...) UseMethod("adjust.previous", object)
bizdays <- function(object, ...) UseMethod("bizdays", object)
is.bizday <- function(object, ...) UseMethod("is.bizday", object)
offset <- function(object, ...) UseMethod("offset", object)

is.Calendar <- function(cal) class(cal) == 'Calendar'

#'
adjust.next.Calendar <- function(cal, date) cal$adjust.next(date)
adjust.previous.Calendar <- function(cal, date) cal$adjust.previous(date)
bizdays.Calendar <- function(cal, from, to) cal$bizdays(from, to)
is.bizday.Calendar <- function(cal, date) cal$is.bizday(date)
seq.Calendar <- function(cal, from, to) cal$seq(from, to)
offset.Calendar <- function(cal, date, n) {
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


