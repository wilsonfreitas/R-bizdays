
#' @title Creates calendars
#' 
#' @description
#' \code{create.calendar} creates calendars and stores them in the calendar register.
#' 
#' @param name calendar's name. This is used to retrieve calendars from register.
#' @param holidays a vector of Dates which contains the holidays
#' @param weekdays a character vector which defines the weekdays to be used as
#' non-working days (defaults to \code{NULL} which represents an actual 
#' calendar). It accepts: \code{sunday}, \code{monday}, \code{thuesday}, 
#' \code{wednesday}, \code{thursday}, \code{friday}, \code{saturday}. 
#' Defining the weekend as nonworking days is \code{weekdays=c("saturday", "sunday")}.
#' @param start.date the date which the calendar starts
#' @param end.date the date which the calendar ends
#' @param adjust.from is a function to be used with the \code{bizdays}'s \code{from} argument.
#' That function adjusts the argument if it is a nonworking day according to calendar.
#' @param adjust.to is a function to be used with the \code{bizdays}'s \code{to} argument.
#' See also \code{adjust.from}.
#' 
#' @details
#' The arguments \code{start.date} and \code{end.date} can be set but once they aren't and \code{holidays}
#' is set, \code{start.date} is defined to \code{min(holidays)} and \code{end.date} to \code{max(holidays)}.
#' If holidays isn't set \code{start.date} is set to \code{'1970-01-01'} and \code{end.date} to \code{'2071-01-01'}.
#' 
#' \code{weekdays} is controversial but it is only a sequence of nonworking weekdays.
#' In the great majority of situations it refers to the weekend but it is also possible defining
#' it differently.
#' \code{weekdays} accepts a \code{character} sequence with lower case weekdays (
#' \code{sunday}, \code{monday}, \code{thuesday}, \code{wednesday}, \code{thursday},
#' \code{friday}, \code{saturday}).
#' This argument defaults to \code{NULL} because the default intended behavior for 
#' \code{create.calendar} returns an \emph{actual} calendar, so calling \code{create.calendar(name="xxx")} 
#' returns a \emph{actual} calendar named \emph{xxx}.
#' (for more calendars see \href{http://en.wikipedia.org/wiki/Day_count_convention}{Day Count Convention})
#' To define the weekend as the nonworking weekdays one could simply
#' use \code{weekdays=c("saturday", "sunday")}.
#' 
#' The arguments \code{adjust.from} and \code{adjust.to} are used to adjust \code{bizdays}' arguments
#' \code{from} and \code{to}, respectively.
#' These arguments need to be adjusted when nonworking days are provided.
#' The default behavior, setting \code{adjust.from=adjust.previous} and \code{adjust.to=adjust.next},
#' works like Excel's function NETWORKDAYS, since that is fairly used by a great number of practitioners.
#' 
#' 
#' @section Calendars register:
#' 
#' Every named calendar is stored in a register so that it can be retrieved by 
#' its name (in \code{calendars}).
#' bizdays' methods also accept the calendar's name on their \code{cal} argument.
#' Given that, naming calendars is strongly recommended.
#' 
#' @seealso
#' \code{\link{calendars}}, \code{\link{bizdays}}
#' 
#' @name create.calendar
#' 
#' @examples
#' # ANBIMA's calendar (from Brazil)
#' cal <- create.calendar("Brazil/ANBIMA", holidays=holidaysANBIMA, weekdays=c("saturday", "sunday"))
#' 
#' # ACTUAL calendar
#' cal <- create.calendar("Actual")
#' 
#' # named calendars can be accessed by its name
#' create.calendar(name="Actual")
#' bizdays('2016-01-01', '2016-03-14', 'Actual')
NULL

#' @export
#' @rdname create.calendar
Calendar <- function(holidays=integer(0),
                     start.date=NULL, end.date=NULL, name=NULL,
                     weekdays=NULL, adjust.from=adjust.next,
                     adjust.to=adjust.previous) {
  warning('This function will be deprecated, use create.calendar instead.')
  Calendar_(holidays, start.date, end.date, name, weekdays, adjust.from, adjust.to)
}

Calendar_ <- function (holidays=integer(0),
                       start.date=NULL, end.date=NULL, name=NULL,
                       weekdays=NULL, adjust.from=adjust.next,
                       adjust.to=adjust.previous) {
  
  if (length(holidays) != 0 && all(is.null(weekdays)))
    warning('You provided holidays without set weekdays.\n',
            'That setup leads to inconsistencies!')
  that <- list()
  # adjust functions
  that$adjust.from <- adjust.from
  that$adjust.to <- adjust.to
  # weekdays
  weekdays_codes <- list(monday=4, tuesday=5, wednesday=6, thursday=0,
                         friday=1, saturday=2, sunday=3)
  wdays <- unlist(weekdays_codes[weekdays])
  wdays <- if (is.null(wdays)) integer(0) else wdays
  that$weekdays <- weekdays
  # name
  that$name <- name
  # dates and holidays
  n.holidays <- as.integer(as.Date(holidays, origin='1970-01-01'))
  # start.date and end.date
  .has_holidays <- length(holidays) != 0
  if (is.null(start.date)) {
    start.date <- if (.has_holidays) as.Date(min(n.holidays), origin='1970-01-01') else as.Date('1970-01-01')
  } else
    start.date <- as.Date(start.date)
  if (is.null(end.date)) {
    end.date <- if (.has_holidays) as.Date(max(n.holidays), origin='1970-01-01') else as.Date('2071-01-01')
  } else
    end.date <- as.Date(end.date)
  that$start.date <- start.date
  that$end.date <- end.date
  n.start.date <- as.integer(start.date)
  n.end.date <- as.integer(end.date)
  # dates
  n.dates <- as.integer(seq(from=start.date, to=end.date, by='day'))
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
#' @rdname create.calendar
create.calendar <- function(name,
                            holidays=integer(0),
                            weekdays=NULL, 
                            start.date=NULL, end.date=NULL,
                            adjust.from=adjust.none, adjust.to=adjust.none) {
  cal <- Calendar_(holidays=holidays, weekdays=weekdays, name=name,
                  start.date=start.date, end.date=end.date,
                  adjust.from=adjust.from, adjust.to=adjust.to)
  .CALENDAR_REGISTER[[cal$name]] <- cal
  invisible(cal)
}

#' @export
print.Calendar <- function(x, ...) {
  cal <- x
  cat('Calendar:', cal$name,
      '\nRange:', format(as.Date(cal$start.date, origin='1970-01-01'), '%Y-%m-%d'),
      'to', format(as.Date(cal$end.date, origin='1970-01-01'), '%Y-%m-%d'),
      '\nweekdays:', cal$weekdays,
      '\n')
  invisible(x)
}

.CALENDAR_REGISTER <- new.env()
class(.CALENDAR_REGISTER) <- 'CalendarRegister'

#' @export
print.CalendarRegister <- function(x, ...) {
  cat('Calendars:', '\n')
  cat(paste(sort(names(.CALENDAR_REGISTER)), collapse=', '))
  cat('\n')
  invisible(.CALENDAR_REGISTER)
}

#' @title Calendars register
#' 
#' @description
#' Every calendar created with \code{create.calendar} is stored in the calendar register.
#' The idea behind this register is allowing calendars to be accessed by its names.
#' 
#' @param cals character vector of calendars names
#' 
#' @details
#' \code{calendars} returns the object which represents the calendars register.
#' Since the register inherits from \code{environment}, the calendars are 
#' retrieved with the \code{[[} operator.
#' But the register object has its own \code{print} generic which helps listing 
#' all registered calendars.
#' 
#' \code{remove.calendars} remove calendars from the register.
#' 
#' @name calendar-register
NULL

#' @export
#' @rdname calendar-register
#' @examples
#' # ACTUAL calendar
#' cal <- create.calendar("Actual")
#' cal <- calendars()[["Actual"]]
#' remove.calendars("Actual")
#' # lists registered calendars
#' calendars()
calendars <- function() {
  .CALENDAR_REGISTER
}

#' @export
#' @rdname calendar-register
remove.calendars <- function(cals) {
  remove(list=cals, envir=.CALENDAR_REGISTER)
}

check_calendar <- function(cal) {
  if ( is.null(cal) )
    stop('Given calendar is NULL.')
  if ( is(cal, 'character') ) {
    if ( is.null(calendars()[[cal]]) )
      stop('Invalid calendar name: ', cal)
    calendars()[[cal]]
  } else if ( is(cal, 'Calendar') )
    cal
  else
    stop('Invalid argument')
}
