
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

test_that('it should return negative bizdays', {
  cal <- Calendar_(holidaysANBIMA, weekdays=c('saturday', 'sunday'))
  expect_equal(bizdays('2014-07-12', '2013-07-12', cal), -251)
  expect_equal(
    bizdays(c('2013-08-21', '2013-01-31', '2013-01-01'), c('2013-08-24', '2013-01-01', '2014-01-01'), cal),
    c(2, -21, 252)
  )
  expect_equal(
    bizdays(c('2013-08-21', '2013-01-31', '2013-01-01'), c(NA, '2013-01-01', '2014-01-01'), cal),
    c(NA, -21, 252)
  )
  cal <- calendars()[["actual"]]
  expect_equal(bizdays(Sys.Date(), Sys.Date()+c(2, -1, 1, 1)), c(2, -1, 1, 1))
})

test_that("it should compute bizdays using double index approach", {
  create.calendar(name = "example1", weekdays = c("saturday", "sunday"),
                  start.date = "2017-01-24", end.date = "2017-01-30", holidays = "2017-01-25")
  expect_equal(
    bizdays("2017-01-24", "2017-01-25", "example1"),
    bizdays("2017-01-25", "2017-01-26", "example1")
  )
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

context('bizdays and current days equivalence')

test_that('it should compute the business days equivalent to current days', {
  cal <- Calendar_(holidaysANBIMA, weekdays=c('saturday', 'sunday'))
  expect_equal(bizdayse('2013-08-21', 3, cal), 2)
})