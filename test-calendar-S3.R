
source('calendar-S3.R')

methods(class='numeric')
methods(class='calendar')

holidaysANDIMA <- as.Date(unlist(read.table('ANBIMA.cal')))
cal <- calendar(holidaysANDIMA)
cal
class(cal)
names(cal)
adjust.next(cal, '2013-01-01')
adjust.previous(cal, '2013-01-01')
bizdays(cal, from='2013-01-01', to='2013-01-05')
is.bizday(cal, '2013-01-01')
is.bizday(cal, '2013-01-02')
seq(cal, '2013-01-01', '2013-01-05')

