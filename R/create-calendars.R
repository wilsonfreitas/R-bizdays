#' Calendars from other packages
#'
#' The packages RQuantLib and timeDate (Rmetrics) have functions to compute
#' business days between 2 dates according to a predefined calendar.
#' bizdays creates calendars based on these functions.
#'
#' @param ql_calendars (QuantLib only) A character vector with the names of
#' QuantLib's calendars. This parameter defaults to NULL, which loads all
#' calendars.
#' @param from (QuantLib only) the start date
#' @param to (QuantLib only) the end date
#' @param year (timeDate Rmetrics only) a vector with years to create the
#' calendars.
#' @param financial is a logical argument that defaults to TRUE.
#'
#'
#' @details
#' To load QuantLib's calendars use \code{load_quantlib_calendars} defining
#' which
#' calendar has to be loaded by its name and the range of dates the calendar
#' has to handle.
#' All QuantLib calendars have the \code{QuantLib} prefix.
#'
#' To load Rmetrics' calendars use \code{load_rmetrics_calendars} defining the
#' years the calendar has to handle.
#' All Rmetrics calendars have the \code{Rmetrics} prefix.
#'
#' @section Financial calendars:
#'
#' This argument defines the calendar as a financial or a non financial
#' calendar.
#' Financial calendars don't consider the ending business day when counting
#' working days in \code{bizdays}.
#' In QuantLib, Financial calendars are those that \code{includeLast} is set
#' to \code{FALSE}.
#'
#' @section List of calendars:
#'
#' QuantLib Calendars:
#'
#' \itemize{
#' \item QuantLib/TARGET
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
#'   # loading Argentina calendar
#'   load_quantlib_calendars("Argentina",
#'     from = "2016-01-01",
#'     to = "2016-12-31"
#'   )
#'   bizdays("2016-01-01", "2016-03-14", "QuantLib/Argentina")
#'   # loading 2 calendars
#'   load_quantlib_calendars(c("UnitedStates/NYSE", "UnitedKingdom/Settlement"),
#'     from = "2016-01-01", to = "2016-12-31"
#'   )
#'   bizdays("2016-01-01", "2016-03-14", "QuantLib/UnitedStates/NYSE")
#'   # loading all QuantLib's 50 calendars
#'   load_quantlib_calendars(from = "2016-01-01", to = "2016-12-31")
#'   bizdays("2016-01-01", "2016-03-14", "QuantLib/Brazil")
#' }
#'
#' if (require("timeDate")) {
#'   # loading all Rmetrics calendar
#'   load_rmetrics_calendars(2016)
#'   bizdays("2016-01-01", "2016-03-14", "Rmetrics/NERC")
#'   bizdays("2016-01-01", "2016-03-14", "Rmetrics/NYSE")
#' }
NULL


#' @rdname other-calendars
#' @export
load_quantlib_calendars <- function(ql_calendars = NULL, from, to,
                                    financial = TRUE) {
  if (!requireNamespace("RQuantLib", quietly = TRUE)) {
    stop("RQuantLib needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (is.null(ql_calendars)) {
    ql_calendars <- c(
      "TARGET", "Argentina", "Australia", "Brazil", "Canada",
      "Canada/Settlement", "Canada/TSX", "China",
      "CzechRepublic",
      "Denmark", "Finland", "Germany",
      "Germany/FrankfurtStockExchange", "Germany/Settlement",
      "Germany/Xetra", "Germany/Eurex", "HongKong", "Hungary",
      "Iceland", "India", "Indonesia", "Italy",
      "Italy/Settlement", "Japan", "Mexico", "NewZealand",
      "Norway",
      "Poland", "Russia", "SaudiArabia", "Singapore",
      "Slovakia",
      "SouthAfrica", "SouthKorea", "SouthKorea/KRX", "Sweden",
      "Switzerland", "Taiwan", "Turkey", "Ukraine",
      "UnitedKingdom",
      "UnitedKingdom/Settlement", "UnitedKingdom/Exchange",
      "UnitedKingdom/Metals", "UnitedStates",
      "UnitedStates/Settlement",
      "UnitedStates/NYSE", "UnitedStates/GovernmentBond",
      "UnitedStates/NERC"
    )
  }
  for (cal in ql_calendars) {
    cal_name <- paste0("QuantLib/", cal)
    # QuantLib drops calendars between releases (e.g. Italy/Exchange), so skip them
    holidays_ <- tryCatch(
      RQuantLib::getHolidayList(cal, as.Date(from), as.Date(to)),
      error = function(e) {
        warning("Calendar ", cal_name, " not loaded: ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    if (is.null(holidays_)) next
    create.calendar(holidays_,
      weekdays = c("saturday", "sunday"),
      name = cal_name,
      start.date = from, end.date = to,
      adjust.from = adjust.next, adjust.to = adjust.next,
      financial = financial
    )
    message("Calendar ", cal_name, " loaded")
  }
}

#' @rdname other-calendars
#' @export
load_rmetrics_calendars <- function(year, financial = TRUE) {
  if (!requireNamespace("timeDate", quietly = TRUE)) {
    stop("timeDate needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # get first and last given years to create start and end dates
  fl_year <- range(year)
  start_date <- as.Date(paste0(fl_year[1], "-01-01"))
  end_date <- as.Date(paste0(fl_year[2], "-12-31"))

  holidays_ <- as.Date(timeDate::holidayLONDON(year))
  cal_name <- "Rmetrics/LONDON"
  create.calendar(holidays_,
    weekdays = c("saturday", "sunday"),
    name = cal_name,
    adjust.from = adjust.next, adjust.to = adjust.previous,
    start.date = start_date, end.date = end_date,
    financial = financial
  )
  message("Calendar ", cal_name, " loaded")

  holidays_ <- as.Date(timeDate::holidayNERC(year))
  cal_name <- "Rmetrics/NERC"
  create.calendar(holidays_,
    weekdays = c("saturday", "sunday"),
    name = cal_name,
    adjust.from = adjust.next, adjust.to = adjust.previous,
    start.date = start_date, end.date = end_date
  )
  message("Calendar ", cal_name, " loaded")

  holidays_ <- as.Date(timeDate::holidayNYSE(year))
  cal_name <- "Rmetrics/NYSE"
  create.calendar(holidays_,
    weekdays = c("saturday", "sunday"),
    name = cal_name,
    adjust.from = adjust.next, adjust.to = adjust.previous,
    start.date = start_date, end.date = end_date
  )
  message("Calendar ", cal_name, " loaded")

  holidays_ <- as.Date(timeDate::holidayTSX(year))
  cal_name <- "Rmetrics/TSX"
  create.calendar(holidays_,
    weekdays = c("saturday", "sunday"),
    name = cal_name,
    adjust.from = adjust.next, adjust.to = adjust.previous,
    start.date = start_date, end.date = end_date
  )
  message("Calendar ", cal_name, " loaded")

  holidays_ <- as.Date(timeDate::holidayZURICH(year))
  cal_name <- "Rmetrics/ZURICH"
  create.calendar(holidays_,
    weekdays = c("saturday", "sunday"),
    name = cal_name,
    adjust.from = adjust.next, adjust.to = adjust.previous,
    start.date = start_date, end.date = end_date
  )
  message("Calendar ", cal_name, " loaded")
}
#' Calendars from the Nager.Date public holidays API
#'
#' Creates a calendar with the public holidays of a country downloaded from
#' \url{https://date.nager.at}, a free API with holidays for about 200
#' countries.
#'
#' @param country the country code with 2 letters (ISO 3166-1 alpha-2), like
#'   \code{"BR"} or \code{"US"}.
#' @param years the years to download. All years from \code{min(years)} to
#'   \code{max(years)} are downloaded, so the calendar has no gaps.
#' @param name the calendar's name, defaults to \code{"Nager/<country>"}.
#' @param weekdays the nonworking weekdays (see \code{\link{create.calendar}}).
#'   Nager.Date does not provide them, so set them for countries whose weekend
#'   is not Saturday and Sunday.
#' @param subdivision an optional region code, like \code{"BR-SP"} or
#'   \code{"US-NY"}. Its regional holidays are added to the national ones.
#' @param financial is a logical argument that defaults to TRUE
#'   (see \code{\link{create.calendar}}).
#'
#' @details
#' Only national holidays are used, plus the holidays of \code{subdivision}
#' if given.
#' The calendar covers from January 1st of the first year to December 31st of
#' the last year and uses \code{adjust.from=following} and
#' \code{adjust.to=preceding}, like the built-in calendars.
#'
#' These are public holidays, not exchange calendars: special market closures
#' and half days are not included.
#' Future years are computed from the current rules, so new holidays only
#' appear once Nager.Date adds them.
#'
#' This function needs internet access. Use \code{\link{save_calendar}} to
#' keep the calendar and \code{\link{load_calendar}} to use it offline.
#'
#' @return The calendar, invisibly. It is also stored in the calendar register.
#'
#' @seealso \code{\link{create.calendar}}, \code{\link{save_calendar}}
#'
#' @examples
#' \donttest{
#' load_nager_calendar("BR", 2024:2025)
#' bizdays("2024-01-01", "2024-12-31", "Nager/BR")
#' # with the holidays of the state of Sao Paulo
#' load_nager_calendar("BR", 2024, name = "Nager/BR-SP", subdivision = "BR-SP")
#' }
#' @export
load_nager_calendar <- function(country, years,
                                name = paste0("Nager/", toupper(country)),
                                weekdays = c("saturday", "sunday"),
                                subdivision = NULL, financial = TRUE) {
  if (!is.character(country) || length(country) != 1 ||
    !grepl("^[A-Za-z]{2}$", country)) {
    stop("country must be a 2-letter country code, like \"BR\".", call. = FALSE)
  }
  if (!is.numeric(years) || !length(years) || anyNA(years)) {
    stop("years must be a vector of years without NA.", call. = FALSE)
  }
  country <- toupper(country)
  years <- seq(as.integer(min(years)), as.integer(max(years)))
  holidays_ <- lapply(years, function(year) {
    url <- sprintf(
      "https://date.nager.at/api/v3/PublicHolidays/%d/%s", year, country
    )
    # url() reports the HTTP status as a warning before failing
    x <- tryCatch(jsonlite::fromJSON(url), warning = identity, error = identity)
    if (inherits(x, "condition")) {
      stop("Could not get the holidays of ", country, " in ", year,
        " from date.nager.at: ", conditionMessage(x),
        call. = FALSE
      )
    }
    nager_holidays(x, subdivision)
  })
  holidays_ <- sort(unique(do.call(c, holidays_)))
  cal <- create.calendar(name,
    holidays = holidays_,
    weekdays = weekdays,
    start.date = as.Date(paste0(min(years), "-01-01")),
    end.date = as.Date(paste0(max(years), "-12-31")),
    adjust.from = adjust.next, adjust.to = adjust.previous,
    financial = financial
  )
  message("Calendar ", name, " loaded")
  invisible(cal)
}

# national holidays, plus the ones of subdivision, from a parsed API response
nager_holidays <- function(x, subdivision = NULL) {
  if (!length(x)) {
    return(as.Date(character(0)))
  }
  regional <- vapply(x$counties, function(c) any(subdivision %in% c), logical(1))
  as.Date(x$date[x$global | regional])
}
