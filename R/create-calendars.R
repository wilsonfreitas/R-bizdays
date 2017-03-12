#' Calendars from other packages
#' 
#' The packages RQuantLib and timeDate (Rmetrics) have functions to compute business days between 2 dates according to a pre-defined calendar.
#' bizdays creates calendars based on these functions.
#' 
#' @param ql_calendars (QuantLib only) A character vector with the names of QuantLib's calendars. This parameter defaults to NULL, which loads all calendars.
#' @param from (QuantLib only) the start date
#' @param to (QuantLib only) the end date
#' @param year (timeDate Rmetrics only) a vector with years to create the calendars.
#' 
#' @details
#' To load QuantLib's calendars use \code{load_quantlib_calendars} defining which
#' calendar has to be loaded by its name and the range of dates the calendar has to handle.
#' All QuantLib calendars have the \code{QuantLib} preffix.
#' 
#' To load Rmetrics' calendars use \code{load_rmetrics_calendars} defining the 
#' years the calendar has to handle.
#' All Rmetrics calendars have the \code{Rmetrics} preffix.
#' 
#' @section List of calendars:
#' 
#' QuantLib Calendars:
#' 
#' \itemize{
#' \item QuantLib/Argentina
#' \item QuantLib/Australia
#' \item QuantLib/Brazil
#' \item QuantLib/Canada
#' \item QuantLib/Canada/Settlement
#' \item QuantLib/Canada/TSX
#' \item QuantLib/China
#' \item QuantLib/CzechRepublic
#' \item QuantLib/Denmark
#' \item QuantLib/Finland
#' \item QuantLib/Germany
#' \item QuantLib/Germany/FrankfurtStockExchange
#' \item QuantLib/Germany/Settlement
#' \item QuantLib/Germany/Xetra
#' \item QuantLib/Germany/Eurex
#' \item QuantLib/HongKong
#' \item QuantLib/Hungary
#' \item QuantLib/Iceland
#' \item QuantLib/India
#' \item QuantLib/Indonesia
#' \item QuantLib/Italy
#' \item QuantLib/Italy/Settlement
#' \item QuantLib/Italy/Exchange
#' \item QuantLib/Japan
#' \item QuantLib/Mexico
#' \item QuantLib/NewZealand
#' \item QuantLib/Norway
#' \item QuantLib/Poland
#' \item QuantLib/Russia
#' \item QuantLib/SaudiArabia
#' \item QuantLib/Singapore
#' \item QuantLib/Slovakia
#' \item QuantLib/SouthAfrica
#' \item QuantLib/SouthKorea
#' \item QuantLib/SouthKorea/KRX
#' \item QuantLib/Sweden
#' \item QuantLib/Switzerland
#' \item QuantLib/Taiwan
#' \item QuantLib/Turkey
#' \item QuantLib/Ukraine
#' \item QuantLib/UnitedKingdom
#' \item QuantLib/UnitedKingdom/Settlement
#' \item QuantLib/UnitedKingdom/Exchange
#' \item QuantLib/UnitedKingdom/Metals
#' \item QuantLib/UnitedStates
#' \item QuantLib/UnitedStates/Settlement
#' \item QuantLib/UnitedStates/NYSE
#' \item QuantLib/UnitedStates/GovernmentBond
#' \item QuantLib/UnitedStates/NERC
#' }
#' 
#' Rmetrics Calendars:
#' 
#' \itemize{
#' \item Calendar Rmetrics/LONDON
#' \item Calendar Rmetrics/NERC
#' \item Calendar Rmetrics/NYSE
#' \item Calendar Rmetrics/TSX
#' \item Calendar Rmetrics/ZURICH
#' }
#' 
#' @name other-calendars
#' @examples 
#' if (require("RQuantLib")) {
#'  # loading Argentina calendar
#'  load_quantlib_calendars('Argentina', from='2016-01-01', to='2016-12-31')
#'  bizdays('2016-01-01', '2016-03-14', 'QuantLib/Argentina')
#'  # loading 2 calendars
#'  load_quantlib_calendars(c('UnitedStates/NYSE', 'UnitedKingdom/Settlement'),
#'                          from='2016-01-01', to='2016-12-31')
#'  bizdays('2016-01-01', '2016-03-14', 'QuantLib/UnitedStates/NYSE')
#'  # loading all QuantLib's 49 calendars
#'  load_quantlib_calendars(from='2016-01-01', to='2016-12-31')
#'  bizdays('2016-01-01', '2016-03-14', 'QuantLib/Brazil')
#' }
#' 
#' if (require("timeDate")) {
#'  # loading all Rmetrics calendar
#'  load_rmetrics_calendars(2016)
#'  bizdays('2016-01-01', '2016-03-14', 'Rmetrics/NERC')
#'  bizdays('2016-01-01', '2016-03-14', 'Rmetrics/NYSE')
#' }
NULL

# Registering calendars

local({
  create.calendar("actual")
  localenv <- new.env()
  data("holidaysANBIMA", envir=localenv)
  create.calendar("Brazil/ANBIMA", localenv$holidaysANBIMA,
                  weekdays=c('saturday', 'sunday'),
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  create.calendar("weekends", weekdays=c('saturday', 'sunday'),
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  bizdays.options$set(default.calendar='actual')
})

#' @rdname other-calendars
#' @export
load_quantlib_calendars <- function(ql_calendars=NULL, from, to) {
  if (!requireNamespace("RQuantLib", quietly = TRUE)) {
    stop("RQuantLib needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.null(ql_calendars))
    ql_calendars <- c("Argentina", "Australia", "Brazil", "Canada",
                      "Canada/Settlement", "Canada/TSX", "China", "CzechRepublic",
                      "Denmark", "Finland", "Germany",
                      "Germany/FrankfurtStockExchange", "Germany/Settlement",
                      "Germany/Xetra", "Germany/Eurex", "HongKong", "Hungary",
                      "Iceland", "India", "Indonesia", "Italy", "Italy/Settlement",
                      "Italy/Exchange", "Japan", "Mexico", "NewZealand", "Norway",
                      "Poland", "Russia", "SaudiArabia", "Singapore", "Slovakia",
                      "SouthAfrica", "SouthKorea", "SouthKorea/KRX", "Sweden",
                      "Switzerland", "Taiwan", "Turkey", "Ukraine", "UnitedKingdom",
                      "UnitedKingdom/Settlement", "UnitedKingdom/Exchange",
                      "UnitedKingdom/Metals", "UnitedStates", "UnitedStates/Settlement",
                      "UnitedStates/NYSE", "UnitedStates/GovernmentBond", "UnitedStates/NERC")
  for (cal in ql_calendars) {
    cal_name <- paste0("QuantLib/", cal)
    holidays_ <- RQuantLib::getHolidayList(cal, as.Date(from), as.Date(to))
    create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name,
                    start.date=from, end.date=to,
                    adjust.from=adjust.next, adjust.to=adjust.next)
    message("Calendar ", cal_name, " loaded")
  }
}

#' @rdname other-calendars
#' @export
load_rmetrics_calendars <- function(year) {
  if (!requireNamespace("timeDate", quietly = TRUE)) {
    stop("timeDate needed for this function to work. Please install it.",
         call. = FALSE)
  }
  holidays_ <- as.Date(timeDate::holidayLONDON(year))
  cal_name <- "Rmetrics/LONDON"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name,
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayNERC(year))
  cal_name <- "Rmetrics/NERC"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name,
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayNYSE(year))
  cal_name <- "Rmetrics/NYSE"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name,
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayTSX(year))
  cal_name <- "Rmetrics/TSX"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name,
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayZURICH(year))
  cal_name <- "Rmetrics/ZURICH"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name,
                  adjust.from=adjust.next, adjust.to=adjust.previous)
  message("Calendar ", cal_name, " loaded")
}
