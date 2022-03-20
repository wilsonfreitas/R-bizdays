
# Registering calendars

.onAttach <- function(libname, pkgname) {
  create.calendar("actual")
  localenv <- new.env()
  utils::data("holidaysANBIMA", envir = localenv)
  create.calendar("Brazil/ANBIMA", localenv$holidaysANBIMA,
                  weekdays = c("saturday", "sunday"),
                  adjust.from = adjust.next, adjust.to = adjust.previous)
  utils::data("holidaysB3", envir = localenv)
  create.calendar("Brazil/B3", localenv$holidaysB3,
                  weekdays = c("saturday", "sunday"),
                  adjust.from = adjust.next, adjust.to = adjust.previous)
  create.calendar("weekends", weekdays = c("saturday", "sunday"),
                  adjust.from = adjust.next, adjust.to = adjust.previous)
  bizdays.options$set(default.calendar = "actual")
}
