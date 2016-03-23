
# Registering calendars

(function() {
  create.calendar(name="actual/365", dib=365)
  create.calendar(name="actual/360", dib=360)
  localenv <- new.env()
  data("holidaysANBIMA", envir=localenv)
  create.calendar(localenv$holidaysANBIMA, weekdays=c('saturday', 'sunday'), name="business/252 ANBIMA", dib=252)
  bizdays.options$set(default.calendar='actual/365')
})()

