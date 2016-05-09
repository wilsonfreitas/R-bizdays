
# Registering calendars

# RQuantLib Calendars
# Argentina, Australia, Brazil, Canada and Canada/Settlement, Canada/TSX, China, CzechRepublic, Denmark, Finland, Germany and Germany/FrankfurtStockExchange, Germany/Settlement, Germany/Xetra, Germany/Eurex, HongKong, Hungary, Iceland, India, Indonesia, Italy and Italy/Settlement, Italy/Exchange, Japan, Mexico, NewZealand, Norway, Poland, Russia, SaudiArabia, Singapore, Slovakia, SouthAfrica, SouthKorea, SouthKorea/KRX, Sweden, Switzerland, Taiwan, Turkey, Ukraine, UnitedKingdom and UnitedKingdom/Settlement, UnitedKingdom/Exchange, UnitedKingdom/Metals, UnitedStates and UnitedStates/Settlement, UnitedStates/NYSE, UnitedStates/GovernmentBond, UnitedStates/NERC and WeekendsOnly

local({
  create.calendar("actual")
  localenv <- new.env()
  data("holidaysANBIMA", envir=localenv)
  create.calendar("ANBIMA", localenv$holidaysANBIMA, weekdays=c('saturday', 'sunday'))
  create.calendar("weekends", weekdays=c('saturday', 'sunday'))
  bizdays.options$set(default.calendar='actual')
})

#' @export
load_quantlib_calendars <- function(ql_calendars=NULL, from, to) {
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
    holidays_ <- RQuantLib::getHolidayList("Brazil", as.Date(from), as.Date(to))
    create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name)
    message("Calendar ", cal_name, " loaded")
  }
}

#' @export
load_rmetrics_calendars <- function(year) {
  holidays_ <- as.Date(timeDate::holidayLONDON(year))
  cal_name <- "Rmetrics/LONDON"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayNERC(year))
  cal_name <- "Rmetrics/NERC"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayNYSE(year))
  cal_name <- "Rmetrics/NYSE"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayTSX(year))
  cal_name <- "Rmetrics/TSX"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name)
  message("Calendar ", cal_name, " loaded")
  
  holidays_ <- as.Date(timeDate::holidayZURICH(year))
  cal_name <- "Rmetrics/ZURICH"
  create.calendar(holidays_, weekdays=c('saturday', 'sunday'), name=cal_name)
  message("Calendar ", cal_name, " loaded")
}
