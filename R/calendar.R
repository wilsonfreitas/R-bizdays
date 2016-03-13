
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
                      start.date=NULL, end.date=NULL, name=NULL,
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
  if (!is.null(that$name))
    .CALENDAR_REGISTER[[that$name]] <- that
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

.CALENDAR_REGISTER <- new.env()
class(.CALENDAR_REGISTER) <- 'CalendarRegister'

print.CalendarRegister <- function(x, ...) {
  cat('Calendars:', '\n')
  for (n in names(.CALENDAR_REGISTER))
    cat(n, '\n')
  invisible(.CALENDAR_REGISTER)
}

calendars <- function() {
  .CALENDAR_REGISTER
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
