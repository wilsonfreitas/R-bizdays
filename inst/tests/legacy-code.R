
test_that("is business day", {
    expect_false(is.bizday(cal, as.Date('2013-01-01')))
    expect_true(is.bizday(cal, as.Date('2013-01-02')))
    dates <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
    expect_equal(is.bizday(cal, dates), c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

context('adjustment of business days')

test_that("next business day", {
    date <- as.character(adjust.next(cal, '2013-01-01'))
    expect_equal(date, '2013-01-02')
})

test_that("previous business day", {
    date <- as.character(adjust.previous(cal, '2013-02-02'))
    expect_equal(date, '2013-02-01')
})

context('sequence of bizdays')

test_that("sequence of bizdays", {
    s <- c("2013-01-02","2013-01-03","2013-01-04","2013-01-07","2013-01-08",
    "2013-01-09","2013-01-10")
    expect_true(all( bizseq(cal, '2013-01-01', '2013-01-10') == s ))
})

context('offset by a number of business days')

test_that("it should offset the date by n business days", {
    expect_equal(add(cal, '2013-01-02', 1), as.Date('2013-01-03'))
    expect_equal(add(cal, '2013-01-02', 3), as.Date('2013-01-07'))
    expect_equal(add(cal, '2013-01-02', 0), as.Date('2013-01-02'))
    expect_equal(add(cal, '2013-01-01', 0), as.Date('2013-01-02'))
    expect_equal(add(cal, '2013-01-01', -1), as.Date('2012-12-28'), label=add(cal, '2013-01-01', -1))
    dates <- c(as.Date('2013-01-01'), as.Date('2013-01-02'))
    expect_equal(add(cal, dates, 1), c(as.Date('2013-01-03'), as.Date('2013-01-03')))
    cal <- Calendar(start.date='2013-01-01', end.date='2013-01-31')
    # expect_error(add(cal, '2013-01-10', 30), 'Given date out of range.')
})

context('vectorized operations')

test_that('it should adjust.next a vector of dates', {
    dates <- c(as.Date('2013-01-01'), as.Date('2013-01-02'))
    adj.dates <- adjust.next(cal, dates)
    expect_equal(adj.dates, c(as.Date('2013-01-02'), as.Date('2013-01-02')))
})

test_that('it should adjust.previous a vector of dates', {
    dates <- c(as.Date('2013-01-01'), as.Date('2013-01-02'))
    adj.dates <- adjust.previous(cal, dates)
    expect_equal(adj.dates, c(as.Date('2012-12-31'), as.Date('2013-01-02')))
})

context('naming calendars')

test_that('it should name a Calendar instance', {
	cal <- Calendar(name='ANBIMA', holidays=holidaysANBIMA)
	expect_equal(name(cal), 'ANBIMA')
	
	cal <- Calendar()
	expect_true(is.null(name(cal)))
})

context('setting options')

test_that('it should set the default calendar', {
	cal <- Calendar(name='Weekdays')
	expect_true( is.null(bizdays.options$get('default.calendar')) )
	bizdays.options$set(default.calendar=cal)
	expect_equal(name(cal), name(bizdays.options$get('default.calendar')))
})

context('bizdays calls')

test_that('it should call bizdays.default with default calendar', {
	bizdays.options$set(default.calendar=cal)
	expect_equal(bizdays('2013-07-12', '2014-07-12'), 251)
	expect_equal(bizdays(as.Date('2013-07-12'), '2014-07-12'), 251)
})

test_that('it should call bizdays.default with no default calendar', {
	bizdays.options$set(default.calendar=NULL)
	expect_error(bizdays('2013-07-12', '2014-07-12'))
})

