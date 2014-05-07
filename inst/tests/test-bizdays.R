
context('bizdays with default calendar')

test_that('bizdays using default calendar', {
	# The default Calendar has no holidays and weekends as non-working days
	# saturday and sunday
	expect_equal(bizdays('2013-01-02', '2013-01-03'), 1)
	expect_equal(bizdays(as.Date('2013-01-02'), '2013-01-03'), 1)
	expect_equal(bizdays(as.Date('2013-01-02'), as.Date('2013-01-03')), 1)
	expect_error(bizdays('2013-31-02', '2013-01-03'))
})

test_that('it should pass Date sequences to bizdays', {
	expect_equal(bizdays(c('2013-01-02', '2013-01-03'), '2013-01-03'), c(1, 0))
})

test_that('it should bizdays a set of dates', {
	dates.from <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
	dates.to <- dates.from + 5
	expect_equal(bizdays(dates.from, dates.to), c(3, 3, 3, 3, 3))
	expect_equal(bizdays('2013-01-02', dates.to), c(2, 3, 4, 5, 6))
	expect_equal(bizdays(dates.from, '2013-01-08'), c(5, 4, 3, 2, 1))
	expect_error(bizdays(c('2013-01-08', '2013-01-08', '2013-01-08'),
		c('2013-01-08', '2013-01-08')),
		"from's length must be multiple of to's length and vice-versa.")
})

test_that('it should bizdays NA values', {
  expect_equal(bizdays('2013-01-01', c('2013-12-31', '2014-12-31', NA)), c(260, 521, NA))
  expect_equal(adjust.next(c('2013-12-31', '2014-12-31', NA)), as.Date(c('2013-12-31', '2014-12-31', NA)))
  expect_equal(adjust.previous(c('2013-12-31', '2014-12-31', NA)), as.Date(c('2013-12-31', '2014-12-31', NA)))
  expect_equal(is.bizday(c('2013-12-31', '2014-12-31', NA)), c(TRUE, TRUE, NA))
})