

calendar <- function (holidays) {
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
	that$seqi <- function(from, to) {
		date <- from
		iter <- list()
		iter$has.next <- function() that$adjust.next(date) <= to
		iter$get.next <- function() {
			if ( ! iter$has.next() ) stop("Stop iteration.")
			curr.date <- that$adjust.next(date)
			date <<- curr.date + 1
			return(curr.date)
		}
		return(iter)
	}
	
	class(that) <- 'calendar'
	return(that)
}

adjust.next <- function(cal, date, ...) UseMethod("adjust.next")
adjust.previous <- function(cal, date, ...) UseMethod("adjust.previous")
bizdays <- function(cal, from, to, ...) UseMethod("bizdays")
is.bizday <- function(cal, date, ...) UseMethod("is.bizday")

adjust.next.calendar <- function(cal, date) cal$adjust.next(date)
adjust.previous.calendar <- function(cal, date) cal$adjust.previous(date)
bizdays.calendar <- function(cal, from, to) cal$bizdays(from, to)
is.bizday.calendar <- function(cal, date) cal$is.bizday(date)
print.calendar <- function(cal) {
	cat("calendar class", "\n")
}
seq.calendar <- function(cal, from, to) cal$seq(from, to)


