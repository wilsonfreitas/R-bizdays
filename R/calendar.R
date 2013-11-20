 
#' Creates the calendar based on a list of holidays.
#' 
#' Calendar is the main class, it has all attributes necessary to execute
#' business days calculations.
#'
#' @param holidays a vector of Dates which contains the holidays
#' @param start.date the date which calendar starts
#' @param end.date the date which calendar ends
#' @param name calendar's name
#' @export
#' @examples
#' # holidays has iso-formated dates
#' data(holidaysANBIMA)
#' cal <- Calendar(name='ANBIMA', holidays=holidaysANBIMA)
#' # For empty calendar just pass nothing
#' cal <- Calendar(name='Weekdays') # from 1970-01-01 to 2071-01-01
#' # unnamed calendars have NULL names
#' cal <- Calendar(start.date='1976-07-12', end.date='2013-10-28')
#' is.null(name(cal)) # TRUE
Calendar <- function (holidays=integer(0),
		start.date='1970-01-01', end.date='2071-01-01', name=NULL,
		weekdays=c('saturday', 'sunday')) {
	
	that <- list()
	# weekdays
	weekdays_codes <- list(monday=4, tuesday=5, wednesday=6, thursday=0,
		friday=1, saturday=2, sunday=3)
	wdays <- unlist(weekdays_codes[weekdays])
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
	#offset
	that$offset <- function(date, n) {
		if (n >= 0) {
			adjust <- function(date) .adjust(date, 1L)
			date <- adjust(date)
			inc <- 1L
		} else {
			adjust <- function(date) .adjust(date, -1L)
			date <- adjust(date)
			inc <- -1L
			n <- abs(n)
		}
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


#' @S3method weekdays Calendar
weekdays.Calendar <- function(x, ...) x$weekdays

#' @S3method print Calendar
print.Calendar <- function(x, ...) {
	cal <- x
	cat('Calendar:', name(cal),
		'\nRange:', format(as.Date(cal$start.date, origin='1970-01-01'), '%Y-%m-%d'),
		'to', format(as.Date(cal$end.date, origin='1970-01-01'), '%Y-%m-%d'))
}

# is.Calendar <- function(cal) class(cal) == 'Calendar'
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
adjust.next <- function(object, ...) UseMethod("adjust.next", object)

#' @rdname adjust.next
#' @method adjust.next Calendar
#' @S3method adjust.next Calendar
adjust.next.Calendar <- function(cal, dates) {
	dates <- as.Date(dates)
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- as.integer(dates)
	as.Date(cal$adjust.next(dates), origin='1970-01-01')
}

#' @rdname adjust.next
#' @method adjust.next character
#' @S3method adjust.next character
adjust.next.character <- function(dates) {
	dates <- as.Date(dates)
	adjust.next(dates)
}

#' @rdname adjust.next
#' @method adjust.next Date
#' @S3method adjust.next Date
adjust.next.Date <- function(dates) {
	cal <- bizdays.options$get('default.calendar')
	adjust.next(cal, dates)
}

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
adjust.previous <- function(object, ...) UseMethod("adjust.previous", object)

#' @rdname adjust.previous
#' @method adjust.previous Calendar
#' @S3method adjust.previous Calendar
adjust.previous.Calendar <- function(cal, dates) {
	dates <- as.Date(dates)
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- as.integer(dates)
	as.Date(cal$adjust.previous(dates), origin='1970-01-01')
}

#' @rdname adjust.previous
#' @method adjust.previous character
#' @S3method adjust.previous character
adjust.previous.character <- function(dates) {
	dates <- as.Date(dates)
	adjust.previous(dates)
}

#' @rdname adjust.previous
#' @method adjust.previous Date
#' @S3method adjust.previous Date
adjust.previous.Date <- function(dates) {
	cal <- bizdays.options$get('default.calendar')
	adjust.previous(cal, dates)
}

#' Computes business days between two dates.
#'
#' This function computes the amount of business days between 2 taking into
#' account the holidays passed to the Calendar function.
#' 
#' @param ... function parameters
#' @export
# bizdays <- function(...) UseMethod('bizdays')

#' @rdname bizdays
#' @param cal an instance of Calendar
#' @param from the initial date (or a vector of dates)
#' @param to the final date (or a vector of dates).
#' All of these dates must be greater than the initial
#' dates.
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' bizdays(cal, '2013-01-02', '2013-01-31')
#' # Once you have a default calendar set
#' bizdays.options$set(default.calendar=cal)
#' bizdays('2013-01-02', '2013-01-31')
bizdays <- function(obj, ...) UseMethod('bizdays', obj)

#' @rdname bizdays
#' @method bizdays Calendar
#' @S3method bizdays Calendar
bizdays.Calendar <- function(cal, from, to) {
	from <- as.Date(from)
	to <- as.Date(to)
	if ( ! any(from >= cal$start.date & from <= cal$end.date) )
		stop('Given date out of range.')
	if ( ! any(to >= cal$start.date & to <= cal$end.date) )
		stop('Given date out of range.')
	if ( ! all(from <= to) )
		stop('All from dates must be greater than all to dates.')
	lengths <- c(length(from), length(to))
	if (max(lengths) %% min(lengths) != 0)
		stop("from's length must be multiple of to's length and vice-versa.")
	cal$bizdays(as.integer(from), as.integer(to))
}

#' @rdname bizdays
#' @method bizdays character
#' @S3method bizdays character
bizdays.character <- function(from, to) {
	from <- as.Date(from)
	to <- as.Date(to)
	bizdays(from, to)
}

#' @rdname bizdays
#' @method bizdays Date
#' @S3method bizdays Date
bizdays.Date <- function(from, to) {
	cal <- bizdays.options$get('default.calendar')
	to <- as.Date(to)
	bizdays(cal, from, to)
}

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
is.bizday <- function(object, ...) UseMethod("is.bizday", object)

#' @rdname is.bizday
#' @method is.bizday Calendar
#' @S3method is.bizday Calendar
is.bizday.Calendar <- function(cal, dates) {
	dates <- as.Date(dates)
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	cal$is.bizday(as.integer(dates))
}

#' @rdname is.bizday
#' @method is.bizday character
#' @S3method is.bizday character
is.bizday.character <- function(dates) {
	dates <- as.Date(dates)
	is.bizday(dates)
}

#' @rdname is.bizday
#' @method is.bizday Date
#' @S3method is.bizday Date
is.bizday.Date <- function(dates) {
	cal <- bizdays.options$get('default.calendar')
	is.bizday(cal, dates)
}

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
bizseq <- function(cal, from, to) {
	from <- as.Date(from)
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

#' Offset the date by n business days.
#'
#' This function returns the given date offset by the given amount of n business
#' days.
#' @param ... isn't used in that implementation
#' @export
offset <- function(obj, ...) UseMethod('offset', obj)

#' @rdname offset
#' @param obj an instance of Calendar
#' @param dates a date or a vector of dates to be offset
#' @param n the amount of business days to offset
#' @method offset Calendar
#' @S3method offset Calendar
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA)
#' offset(cal, '2013-01-02', 5)
#' dates <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
#' offset(cal, dates, 1)
offset.Calendar <- function(obj, dates, n, ...) {
	dates <- as.Date(dates)
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- as.integer(dates)
	as.Date(obj$offset(dates, n))
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

#' Calendar's name
#' 
#' Returns calendar's name
#' 
#' @param obj an instance of Calendar
#' @export
#' @examples
#' cal <- Calendar(holidaysANBIMA)
#' name(cal)
name <- function(obj) UseMethod('name', obj)

#' @rdname name
#' @method name Calendar
#' @S3method name Calendar
name.Calendar <- function(obj) obj$name

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
#' \code{bizdays.options} defines the default calendar to be used at 
#' \code{bizdays.default} calls.
#' 
#' @export
#' @examples
#' cal <- Calendar(name='Weekdays')
#' bizdays.options$set(default.calendar=cal)
#' bizdays.options$get('default.calendar')
bizdays.options <- new_defaults(list())

