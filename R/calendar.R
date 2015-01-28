 
#' @title Creates a calendar
#' 
#' @description
#' The \code{Calendar} stores all information necessary to compute business days.
#' This works like a helper class for many of \code{bizdays}' methods.
#' 
#' @param holidays a vector of Dates which contains the holidays
#' @param start.date the date which the calendar starts
#' @param end.date the date which the calendar ends
#' @param name calendar's name
#' @param weekdays a character vector which defines the weekdays to be used as
#' non-working days (defaults to \code{NULL} which represents an actual 
#' calendar). It accepts: \code{sunday}, \code{monday}, \code{thuesday}, 
#' \code{wednesday}, \code{thursday}, \code{friday}, \code{saturday}. 
#' Defining the weekend as nonworking days is \code{weekdays=c("saturday", "sunday")}.
#' @param dib a single numeric variable which indicates the amount of days
#' within a year (\code{dib} stands for days in base).
#' @param adjust.from is a function to be used with the \code{bizdays}'s \code{from} argument.
#' That function adjusts the argument if it is a nonworking day according to calendar.
#' @param adjust.to is a function to be used with the \code{bizdays}'s \code{to} argument.
#' See also \code{adjust.from}.
#' 
#' @details
#' The arguments \code{start.date} and \code{end.date} can be set but once \code{holidays}
#' is set, \code{start.date} is defined to \code{min(holidays)} and \code{end.date} to 
#' \code{max(holidays)}.
#' 
#' \code{weekdays} is controversial but it is only a sequence of nonworking weekdays.
#' In the great majority of situations it refers to the weekend but it is also possible defining
#' it differently.
#' \code{weekdays} accepts a \code{character} sequence with lower case weekdays (
#' \code{sunday}, \code{monday}, \code{thuesday}, \code{wednesday}, \code{thursday},
#' \code{friday}, \code{saturday}).
#' This argument defaults to \code{NULL} because the default intended behavior for 
#' \code{Calendar} returns an \emph{actual} calendar, so calling \code{Calendar(dib=365)} 
#' returns a \emph{actual/365} calendar and \code{Calendar(dib=360)} and \emph{actual/360}
#' (for more calendars see \href{http://en.wikipedia.org/wiki/Day_count_convention}{Day Count Convention})
#' To define the weekend as the nonworking weekdays one could simply
#' use \code{weekdays=c("saturday", "sunday")}.
#' 
#' \code{dib} reffers to \emph{days in base} and represents the amount of days within a year.
#' That is necessary for defining Day Count Conventions and for accounting annualized periods 
#' (see \code{\link{bizyears}}).
#' 
#' The arguments \code{adjust.from} and \code{adjust.to} are used to adjust \code{bizdays}' arguments
#' \code{from} and \code{to}, respectively.
#' These arguments need to be adjusted when nonworking days are provided.
#' The default behavior, setting \code{adjust.from=adjust.previous} and \code{adjust.to=adjust.next},
#' works like Excel's function NETWORKDAYS, since that is fairly used by a great number of practitioners.
#' 
#' \code{Calendar} doesn't have to be named, but it helps identifying the calendars once many are instantiated.
#' You name a \code{Calendar} by setting the argument \code{name}.
#' 
#' @export
#' @examples
#' # holidays has iso-formated dates
#' data(holidaysANBIMA)
#' cal <- Calendar(name="ANBIMA", holidays=holidaysANBIMA,
#'                 weekdays=c("saturday", "sunday"), dib=252)
#' # ACTUAL calendar
#' cal <- Calendar(name="Actual", dib=365)
#' # unnamed calendars have NULL names
#' cal <- Calendar(start.date="1976-07-12", end.date="2013-10-28")
#' is.null(cal$name) # TRUE
Calendar <- function (holidays=integer(0),
		start.date='1970-01-01', end.date='2071-01-01', name=NULL,
		weekdays=NULL, dib=NULL, adjust.from=adjust.next,
		adjust.to=adjust.previous) {
	
	if (length(holidays) != 0 && all(is.null(weekdays)))
		warning('You provided holidays without set weekdays.\n',
				'That setup leads to inconsistencies!')
	that <- list()
	# adjust functions
	that$adjust.from <- adjust.from
	that$adjust.to <- adjust.to
	# dib
	that$dib <- dib
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
		.is.bizday[match(date, n.dates)]
	}
	# bizdays and index
	n.bizdays <- n.dates[.is.bizday]
	index.bizdays <- seq_along(n.bizdays)
	index <- cumsum(.is.bizday)
	# bizdays
	that$bizdays <- function(from, to) {
		from.idx <- index[match(from, n.dates)]
		to.idx <- index[match(to, n.dates)]
		to.idx - from.idx
	}
	# adjust.next and adjust.previous
	.adjust <- function(dates, offset) {
		idx <- .is.bizday[match(dates, n.dates)]
		idx[is.na(idx)] <- TRUE
		while ( ! all(idx) ) {
			dates[!idx] <- dates[!idx] + offset
			idx <- .is.bizday[match(dates, n.dates)]
			idx[is.na(idx)] <- TRUE
		}
		dates
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
		ref <- index.bizdays[which(date == n.bizdays)]
		n.bizdays[which(index.bizdays - ref == n)]
	}
	class(that) <- 'Calendar'
	return(that)
}

#' @export
print.Calendar <- function(x, ...) {
	cal <- x
	cat('Calendar:', cal$name,
		'\nRange:', format(as.Date(cal$start.date, origin='1970-01-01'), '%Y-%m-%d'),
		'to', format(as.Date(cal$end.date, origin='1970-01-01'), '%Y-%m-%d'),
		'\nweekdays:', cal$weekdays,
		'\ndib:', cal$dib,
		'\n')
	invisible(x)
}

#' Adjusts the given dates to the next/previous business day
#'
#' If the given dates are business days it returns the given dates, but once it
#' is not, it returns the next/previous business days.
#'
#' @param dates dates to be adjusted
#' @param cal an instance of \code{Calendar}
#' 
#' @name adjust.date
NULL

#' @rdname adjust.date
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' adjust.next("2013-01-01", cal)
adjust.next <- function(dates, cal=bizdays.options$get('default.calendar')) UseMethod("adjust.next")

#' @export
adjust.next.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	adjust.next(dates, cal)
}

#' @export
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
adjust.previous <- function(dates, cal=bizdays.options$get('default.calendar')) UseMethod("adjust.previous")

#' @export
adjust.previous.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	adjust.previous(dates, cal)
}

#' @export
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
#' @param cal an instance of \code{Calendar}
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' bizdays("2013-01-02", "2013-01-31", cal)
#' # Once you have a default calendar set
#' bizdays.options$set(default.calendar=cal)
#' bizdays("2013-01-02", "2013-01-31")
bizdays <- function(from, to, cal) UseMethod('bizdays')

#' @rdname bizdays
#' @export
bizdays.default <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	from <- as.Date(from)
	bizdays(from, to, cal)
}

#' @rdname bizdays
#' @export
bizdays.Date <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	to <- as.Date(to)
	# ---
	if (all(is.na(to))) return( rep(NA, max(length(to), length(from))) )
	if ( ! any(from >= cal$start.date & from <= cal$end.date) )
		stop('Given date out of range.')
	if ( ! any(to >= cal$start.date & to <= cal$end.date) )
		stop('Given date out of range.')
	lengths <- c(length(from), length(to))
	if (max(lengths) %% min(lengths) != 0)
		stop("from's length must be multiple of to's length and vice-versa.")
	if ( ! all(from <= to, na.rm=TRUE) )
		stop('All from dates must be greater than all to dates.')
	from <- cal$adjust.from(from, cal)
	to <- cal$adjust.to(to, cal)
	cal$bizdays(as.integer(from), as.integer(to))
}

#' Checks if the given date is a business day.
#'
#' This function returns TRUE if the given date is a business day and FALSE
#' otherwise.
#'
#' @param dates a date or a vector of dates to be checked
#' @param cal an instance of \code{Calendar}
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' is.bizday("2013-01-02", cal)
#' # Using the default Calendar
#' dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
#' is.bizday(dates)
is.bizday <- function(dates, cal) UseMethod("is.bizday")

#' @rdname is.bizday
#' @export
is.bizday.default <- function(dates, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  is.bizday(dates, cal)
}

#' @rdname is.bizday
#' @export
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
#' @param cal an instance of \code{Calendar}
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' bizseq("2013-01-02", "2013-01-31", cal)
bizseq <- function(from, to, cal) UseMethod('bizseq')

#' @rdname bizseq
#' @export
bizseq.default <- function(from, to, cal=bizdays.options$get('default.calendar')) {
  from <- as.Date(from)
  bizseq(from, to, cal)
}

#' @rdname bizseq
#' @export
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
#' Returns the given \code{dates} offset by the
#' given amount of \code{n} business days.
#' 
#' @param dates dates to be offset
#' @param n the amount of business days to add
#' @param cal an instance of \code{Calendar}
#' 
#' @details
#' The argument \code{n} accepts a sequence of integers and if its length
#' differs from \code{dates}' length, the recycle rule is applied to fulfill the
#' gap.
#' 
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' add.bizdays("2013-01-02", 5, cal)
#' dates <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by="day")
#' add.bizdays(dates, 1, cal)
add.bizdays <- function(dates, n, cal=bizdays.options$get('default.calendar')) UseMethod('add.bizdays')

#' @export
add.bizdays.default <- function(dates, n, cal=bizdays.options$get('default.calendar')) {
  dates <- as.Date(dates)
  add.bizdays(dates, n, cal)
}

#' @export
add.bizdays.Date <- function(dates, n, cal=bizdays.options$get('default.calendar')) {
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Given date out of range.')
	dates <- cal$adjust.from(dates, cal)
	dates <- as.integer(dates)
	dates <- apply(cbind(dates, n), 1, function(x) cal$add(x[1], x[2]))
	dates <- as.Date(dates, origin='1970-01-01')
	if ( ! any(dates >= cal$start.date & dates <= cal$end.date) )
		stop('Dates out of range')
	dates
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
#' \preformatted{
#' bizdays.options$set(key=value)
#' bizdays.options$get("key")
#' }
#' @usage 
#' bizdays.options
#' @export
#' @examples
#' cal <- Calendar(name="Weekdays")
#' bizdays.options$set(default.calendar=cal)
#' bizdays.options$get("default.calendar")
#' bizdays("2013-07-12", "2013-07-22")
bizdays.options <- new_defaults()
bizdays.options$set(default.calendar=Calendar(name="Actual", dib=365))

#' Computes the period between two dates in years taking into account business days
#' 
#' @param from the initial date (or a vector of dates)
#' @param to the final date (or a vector of dates).
#' @param cal an instance of \code{Calendar}
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"), dib=252)
#' bizyears("2013-01-02", "2013-01-31", cal)
bizyears <- function(from, to, cal) UseMethod('bizyears')

#' @rdname bizyears
#' @export
bizyears.default <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	from <- as.Date(from)
	bizyears(from, to, cal)
}

#' @rdname bizyears
#' @export
bizyears.Date <- function(from, to, cal=bizdays.options$get('default.calendar')) {
	if (is.null(cal$dib))
		stop('NULL dib')
	to <- as.Date(to)
	bizdays(from, to, cal)/cal$dib
}

#' Business days and current days equivalence
#' 
#' \code{bizdayse} stands for business days equivalent, it returns the amount
#' of business days equivalent to a given number of current days.
#' 
#' @param dates the initial date (or a vector of dates)
#' @param curd the amount of current days (or a vector of numeric)
#' @param cal an instance of \code{Calendar}
#' @export
#' @examples
#' data(holidaysANBIMA)
#' cal <- Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday"), dib=252)
#' bizdayse("2013-01-02", 3, cal)
bizdayse <- function(dates, curd, cal) UseMethod('bizdayse')

#' @rdname bizdayse
#' @export
bizdayse.default <- function(dates, curd, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	bizdayse(dates, curd, cal)
}

#' @rdname bizdayse
#' @export
bizdayse.Date <- function(dates, curd, cal=bizdays.options$get('default.calendar')) {
	bizdays(dates, dates+curd, cal)
}

#' @rdname bizdayse
#' @export
#' @examples
#' bizyearse("2013-01-02", 3, cal)
bizyearse <- function(dates, curd, cal) UseMethod('bizyearse')

#' @rdname bizdayse
#' @export
bizyearse.default <- function(dates, curd, cal=bizdays.options$get('default.calendar')) {
	dates <- as.Date(dates)
	bizyearse(dates, curd, cal)
}

#' @rdname bizdayse
#' @export
bizyearse.Date <- function(dates, curd, cal=bizdays.options$get('default.calendar')) {
	bizyears(dates, dates+curd, cal)
}
