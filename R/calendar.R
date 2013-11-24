 
#' Creates the calendar based on a list of holidays.
#' 
#' Calendar is the main class, it has all attributes necessary to execute
#' business days calculations.
#'
#' @param holidays a vector of Dates which contains the holidays
#' @param start.date the date which calendar starts
#' @param end.date the date which calendar ends
#' @param name calendar's name
#' @param weekdays a character vector which defines the weekdays to be used as
#' non-working days (defaults to weekend \code{c('saturday', 'sunday')})
#' @export
#' @examples
#' # holidays has iso-formated dates
#' data(holidaysANBIMA)
#' cal <- Calendar(name="ANBIMA", holidays=holidaysANBIMA)
#' # For empty calendar just pass nothing
#' cal <- Calendar(name="Weekdays") # from 1970-01-01 to 2071-01-01
#' # ACTUAL calendar
#' cal <- Calendar(name="Actual", weekdays=NULL)
#' # unnamed calendars have NULL names
#' cal <- Calendar(start.date="1976-07-12", end.date="2013-10-28")
#' is.null(cal$name) # TRUE
Calendar <- function (holidays=integer(0),
		start.date='1970-01-01', end.date='2071-01-01', name=NULL,
		weekdays=c('saturday', 'sunday')) {
	
	that <- list()
	# weekdays
	weekdays_codes <- list(monday=4, tuesday=5, wednesday=6, thursday=0,
		friday=1, saturday=2, sunday=3)
	wdays <- unlist(weekdays_codes[weekdays])
	wdays <- if (is.null(wdays)) integer(0) else wdays
	that$weekdays <- weekdays
	# name
	that$name <- name
	# start.date and end.date
	start.date <- as.Date(start.date)
	end.date <- as.Date(end.date)
	if (length(holidays) != 0) {
		start.date <- as.Date(min(holidays))
		end.date <- as.Date(max(holidays))
	}
	that$start.date <- start.date
	that$end.date <- end.date
	n.start.date <- as.integer(start.date)
	n.end.date <- as.integer(end.date)
	# dates and holidays
	n.dates <- as.integer(seq(from=start.date, to=end.date, by='day'))
	n.holidays <- as.integer(holidays)
	# is bizday?
	.is.bizday <- vapply(n.dates, function(.) {
		wday <- .%%7
		! ( wday %in% wdays || . %in% n.holidays)
	}, logical(1))
	that$is.bizday <- function(date) {
		.is.bizday[n.dates %in% date]
	}
	# bizdays and index
	n.bizdays <- n.dates[.is.bizday]
	index <- cumsum(.is.bizday)
	# bizdays
	that$bizdays <- function(from, to) {
		apply(cbind(from, to), 1, function(x) {
			from <- x[1]
			to <- x[2]
			from.idx <- index[n.dates %in% that$adjust.next(from)]
			to.idx <- index[n.dates %in% that$adjust.previous(to)]
			stopifnot(length(from.idx) == length(to.idx))
			to.idx - from.idx
		})
	}
	# adjust.next and adjust.previous
	.adjust <- function(dates, offset) {
		vapply(dates, function(date) {
			while ( ! .is.bizday[n.dates == date] ) date <- date + offset
			date
		}, integer(1))
	}
	that$adjust.next <- function(dates) {
		.adjust(dates, 1L)
	}
	that$adjust.previous <- function(dates) {
		.adjust(dates, -1L)
	}
	# seq
	that$seq <- function(from, to) {
		n.bizdays[which(n.bizdays >= from & n.bizdays <= to)]
	}
	# add
	that$add <- function(date, n) {
		if (n >= 0) {
			adjust <- function(date) .adjust(date, 1L)
			inc <- 1L
		} else {
			adjust <- function(date) .adjust(date, -1L)
			inc <- -1L
			n <- abs(n)
		}
		date <- adjust(date)
		i <- 0L
		while (i < n) {
			date <- date + inc
			date <- adjust(date)
			i <- i + 1L
		}
		date
	}
	class(that) <- 'Calendar'
	return(that)
}

#' @S3method print Calendar
print.Calendar <- function(x, ...) {
	cal <- x
	cat('Calendar:', cal$name,
		'\nRange:', format(as.Date(cal$start.date, origin='1970-01-01'), '%Y-%m-%d'),
		'to', format(as.Date(cal$end.date, origin='1970-01-01'), '%Y-%m-%d'))
}

#' Adjusts the date to the next business day
#'
#' Moves the given date to the next business day, once it is a
#' non-business day.
#'
#' @param dates a vector of dates to be adjusted
#' @param cal an instance of Calendar
#' @rdname adjust.date
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' adjust.next("2013-01-01", cal)
adjust.next <- function(dates, cal) UseMethod("adjust.next")

#' @rdname adjust.date
#' @method adjust.next character
#' @S3method adjust.next character
adjust.next.character <- function(dates, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	adjust.next(dates, cal)
}

#' @rdname adjust.date
#' @method adjust.next Date
#' @S3method adjust.next Date
adjust.next.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- as.integer(dates)
	as.Date(cal$adjust.next(dates), origin='1970-01-01')
}

#' @rdname adjust.date
#' @export
#' @examples
#' adjust.previous("2013-01-01", cal)
adjust.previous <- function(dates, cal) UseMethod("adjust.previous")

#' @rdname adjust.date
#' @method adjust.previous character
#' @S3method adjust.previous character
adjust.previous.character <- function(dates, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	adjust.previous(dates, cal)
}

#' @rdname adjust.date
#' @method adjust.previous Date
#' @S3method adjust.previous Date
adjust.previous.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- as.integer(dates)
	as.Date(cal$adjust.previous(dates), origin='1970-01-01')
}

#' Computes business days between two dates.
#'
#' This function computes the amount of business days between 2 taking into
#' account the holidays passed to the Calendar function.
#' 
#' @param from the initial date (or a vector of dates)
#' @param to the final date (or a vector of dates).
#' @param cal an instance of Calendar
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' bizdays("2013-01-02", "2013-01-31", cal)
#' # Once you have a default calendar set
#' bizdays.options$set(default.calendar=cal)
#' bizdays("2013-01-02", "2013-01-31")
bizdays <- function(from, to, cal) UseMethod('bizdays')

#' @rdname bizdays
#' @method bizdays character
#' @S3method bizdays character
bizdays.character <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	from <- as.Date(from)
	to <- as.Date(to)
	bizdays(from, to, cal)
}

#' @rdname bizdays
#' @method bizdays Date
#' @S3method bizdays Date
bizdays.Date <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	to <- as.Date(to)
	# ---
	if ( ! any(from >= cal$start.date & from <= cal$end.date) )
		stop('Given date out of range.')
	if ( ! any(to >= cal$start.date & to <= cal$end.date) )
		stop('Given date out of range.')
	lengths <- c(length(from), length(to))
	if (max(lengths) %% min(lengths) != 0)
		stop("from's length must be multiple of to's length and vice-versa.")
	if ( ! all(from <= to) )
		stop('All from dates must be greater than all to dates.')
	cal$bizdays(as.integer(from), as.integer(to))
}

#' Checks if the given date is a business day.
#'
#' This function returns TRUE if the given date is a business day and FALSE
#' otherwise.
#'
#' @param dates a date or a vector of dates to be checked
#' @param cal an instance of Calendar
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' is.bizday("2013-01-02", cal)
#' # Using the default Calendar
#' dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
#' is.bizday(dates)
is.bizday <- function(dates, cal) UseMethod("is.bizday")

#' @rdname is.bizday
#' @method is.bizday character
#' @S3method is.bizday character
is.bizday.character <- function(dates, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	is.bizday(dates, cal)
}

#' @rdname is.bizday
#' @method is.bizday Date
#' @S3method is.bizday Date
is.bizday.Date <- function(dates, cal=bizdays.options$get('default.calendar')) {
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	cal$is.bizday(as.integer(dates))
}

#' Create a sequence of business days.
#'
#' This function returns a sequence of business days according to the given
#' calendar.
#'
#' @param from the initial date
#' @param to the final date. This date must be greater that the initial date
#' @param cal an instance of Calendar
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' bizseq("2013-01-02", "2013-01-31", cal)
bizseq <- function(from, to, cal) UseMethod('bizseq')

#' @rdname bizseq
#' @method bizseq character
#' @S3method bizseq character
bizseq.character <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	from <- as.Date(from)
	to <- as.Date(to)
	bizseq(from, to, cal)
}

#' @rdname bizseq
#' @method bizseq Date
#' @S3method bizseq Date
bizseq.Date <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	to <- as.Date(to)
	# ---
	to <- as.Date(to)
	if ( ! any(from >= cal$start.date & from <= cal$end.date) )
		stop('Given date out of range.')
	if ( ! any(to >= cal$start.date & to <= cal$end.date) )
		stop('Given date out of range.')
	if ( ! all(from <= to) )
		stop('All from dates must be greater than all to dates.')
	from <- as.integer(from)
	to <- as.integer(to)
	as.Date(cal$seq(from, to), origin='1970-01-01')
}

#' Adds \code{n} business days to the given \code{dates}.
#'
#' This function returns the given \code{dates} offset by the
#' given amount of \code{n} business
#' days.
#' 
#' @param dates a date or a vector of dates to be offset
#' @param n the amount of business days to add
#' @param cal an instance of Calendar
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' add("2013-01-02", 5, cal)
#' dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
#' add(dates, 1, cal)
add <- function(dates, n, cal) UseMethod('add')

#' @rdname add
#' @method add character
#' @S3method add character
add.character <- function(dates, n, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	add(dates, n, cal)
}

#' @rdname add
#' @method add Date
#' @S3method add Date
add.Date <- function(dates, n, cal=bizdays.options$get('default.calendar')) {
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- as.integer(dates)
	as.Date(cal$add(dates, n), origin='1970-01-01')
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

# merge elements of y into x with the same names
merge_list = function(x, y) {
  x[names(y)] = y
  x
}

# new_defaults — creates a settings object
new_defaults <- function(value=list()) {
	defaults <- value

	get <- function(name, default=FALSE, drop=TRUE) {
		if (default)
			defaults <- value  # this is only a local version
		if (missing(name))
			defaults
		else {
			if (drop && length(name) == 1)
				defaults[[name]]
			else
				defaults[name]
			}
		}
	set <- function(...) {
		dots <- list(...)
		if (length(dots) == 0) return()
		if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
		if (length(dots <- dots[[1]]) == 0) return()
		defaults <<- merge(dots)
		invisible(NULL)
	}
	merge <- function(values) merge_list(defaults, values)
	restore <- function(target = value) defaults <<- target

	list(get=get, set=set, merge=merge, restore=restore)
}

#' bizdays' options
#' 
#' \code{bizdays.options} defines the default calendar to be used when
#' calling to the functions: \code{bizdays}, \code{adjust.next}, 
#' \code{adjust.previous}, \code{is.bizday}, \code{bizseq}, \code{offset}; 
#' without providing a \code{Calendar} instance as a parameter.
#' 
#' @format a \code{list} with \code{get} and \code{set} functions attached
#' @usage 
#' bizdays.options$set(key=value)
#' bizdays.options$get("key")
#' @export
#' @examples
#' cal <- Calendar(name="Weekdays")
#' bizdays.options$set(default.calendar=cal)
#' bizdays.options$get("default.calendar")
#' bizdays("2013-07-12", "2013-07-22")
bizdays.options <- new_defaults()
bizdays.options$set(default.calendar=Calendar())

