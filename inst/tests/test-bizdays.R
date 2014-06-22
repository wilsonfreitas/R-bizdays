
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
	expect_equal(bizdays(dates.from, dates.to), c(5, 5, 5, 5, 5))
	expect_equal(bizdays('2013-01-02', dates.to), c(4, 5, 6, 7, 8))
	expect_equal(bizdays(dates.from, '2013-01-08'), c(7, 6, 5, 4, 3))
	expect_error(bizdays(c('2013-01-08', '2013-01-08', '2013-01-08'),
		c('2013-01-08', '2013-01-08')),
		"from's length must be multiple of to's length and vice-versa.")
})

context('handling NA values')

test_that('it should bizdays NA values', {
	expect_equal(bizdays('2013-01-01', c('2013-12-31', '2014-12-31', NA)), c(364, 729, NA))
	expect_equal(adjust.next(c('2013-12-31', '2014-12-31', NA)), as.Date(c('2013-12-31', '2014-12-31', NA)))
	expect_equal(adjust.previous(c('2013-12-31', '2014-12-31', NA)), as.Date(c('2013-12-31', '2014-12-31', NA)))
	expect_equal(is.bizday(c('2013-12-31', '2014-12-31', NA)), c(TRUE, TRUE, NA))
})

test_that('it should bizdays all NA values', {
	expect_equal(bizdays('2013-01-01', NA), NA)
	expect_equal(bizdays(c('2013-01-01', '2013-02-01'), NA), c(NA, NA))
	expect_equal(bizdays('2013-01-01', c(NA, NA)), c(NA, NA))
})

context('bizyears')

test_that('it should bizyears dates', {
	cal <- Calendar(dib=365)
	expect_equal(bizyears('2013-01-02', '2013-01-03', cal), 1/365)
	cal <- Calendar(holidaysANBIMA, dib=252, weekdays=c('saturday', 'sunday'))
	expect_equal(bizyears('2013-08-21', '2013-08-24', cal), 2/252)
})

test_that('it should raise an error while bizyears dates', {
	cal <- Calendar()
	expect_error(bizyears('2013-01-02', '2013-01-03', cal), 'NULL dib')
})

context('bizdays and current days equivalence')

test_that('it should compute the business days equivalent to current days', {
	cal <- Calendar(holidaysANBIMA, dib=252, weekdays=c('saturday', 'sunday'))
	expect_equal(bizdayse('2013-08-21', 3, cal), 2)
	expect_equal(bizyearse('2013-08-21', 3, cal), 2/252)
})