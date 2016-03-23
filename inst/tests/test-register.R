
context('Calendar register')

test_that('it should list calendars thru register', {
  # expect_output(calendars(), 'actual/365')
  # expect_equal(length(calendars()), 1)
  l <- length(calendars())
  cal <- Calendar()
  expect_equal(length(calendars()), l)
  cal <- create.calendar('try-ANBIMA', holidaysANBIMA, weekdays=c('saturday', 'sunday'))
  expect_equal(length(calendars()), l+1)
  expect_output(calendars(), 'try-ANBIMA')
})

test_that('it should retrieve registered calendars', {
  expect_is(calendars()[['actual/365']], 'Calendar')
  expect_null(calendars()[['blá']])
})

test_that('it should call calendar\'s methods with calendar\'s name', {
  expect_error(bizdays('2016-02-01', '2016-02-02', 'actual'), 'Invalid calendar')
  expect_equal(bizdays('2016-02-01', '2016-02-02', 'actual/365'), 1)
  expect_equal(bizyears('2016-02-01', '2016-02-02', 'actual/365'), 1/365)
  expect_equal(is.bizday('2016-02-01', 'actual/365'), TRUE)
  expect_equal(offset('2016-02-01', 1, 'actual/365'), as.Date('2016-02-02'))
  expect_equal(bizseq('2016-02-01', '2016-02-02', 'actual/365'), as.Date(c('2016-02-01', '2016-02-02')))
  expect_equal(modified.following('2013-01-01', 'actual/365'), as.Date('2013-01-01'))
  expect_equal(modified.preceding('2013-01-01', 'actual/365'), as.Date('2013-01-01'))
  expect_equal(following('2013-01-01', 'actual/365'), as.Date('2013-01-01'))
  expect_equal(preceding('2013-01-01', 'actual/365'), as.Date('2013-01-01'))
})

test_that('it should set default calendar with calendar\'s name', {
  cal <- create.calendar("actual/360", dib=360)
  bizdays.options$set(default.calendar='actual/360')
  expect_is(bizdays.options$get('default.calendar'), 'Calendar')
  expect_output(bizdays.options$get('default.calendar'), 'actual/360')
})

test_that('it should remove a calendar', {
  cal <- create.calendar("actual")
  expect_false( is.null(calendars()[["actual"]]) )
  remove.calendars("actual")
  expect_true( is.null(calendars()[["actual"]]) )
})