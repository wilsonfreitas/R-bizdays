
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)

context('handle business days')

test_that("business days counting", {
    expect_equal(bizdays(cal, '2013-07-12', '2014-07-12'), 251)
    expect_equal(bizdays(cal, '2013-08-21', '2013-08-24'), 2)
    expect_equal(bizdays(cal, '2013-01-01', '2013-01-31'), 21)
    expect_equal(bizdays(cal, '2013-01-01', '2014-01-01'), 252)
    expect_equal(bizdays(cal, '2014-01-01', '2015-01-01'), 252)
    expect_equal(bizdays(cal, '2014-10-10', '2015-02-11'), 86)
    expect_equal(bizdays(cal, '2013-08-13', '2013-09-02'), 14)
    expect_equal(bizdays(cal, '2013-08-13', '2013-10-01'), 35)
    expect_equal(bizdays(cal, '2013-08-13', '2013-11-01'), 58)
    expect_equal(bizdays(cal, '2013-08-13', '2013-12-02'), 78)
    expect_equal(bizdays(cal, '2013-08-13', '2014-01-02'), 99)
    expect_equal(bizdays(cal, '2013-08-13', '2014-04-01'), 160)
    expect_equal(bizdays(cal, '2013-08-13', '2014-07-01'), 221)
    expect_equal(bizdays(cal, '2013-08-13', '2014-10-01'), 287)
    expect_equal(bizdays(cal, '2013-08-13', '2015-01-02'), 352)
    expect_equal(bizdays(cal, '2013-08-13', '2015-04-01'), 413)
    expect_equal(bizdays(cal, '2013-08-13', '2015-07-01'), 474)
    expect_equal(bizdays(cal, '2013-08-13', '2015-10-01'), 539)
    expect_equal(bizdays(cal, '2013-08-13', '2016-01-04'), 602)
    expect_equal(bizdays(cal, '2013-08-13', '2016-04-01'), 663)
    expect_equal(bizdays(cal, '2013-08-13', '2016-07-01'), 726)
    expect_equal(bizdays(cal, '2013-08-13', '2016-10-03'), 791)
    expect_equal(bizdays(cal, '2013-08-13', '2017-01-02'), 853)
    expect_equal(bizdays(cal, '2013-08-13', '2017-04-03'), 916)
    expect_equal(bizdays(cal, '2013-08-13', '2017-07-03'), 977)
    expect_equal(bizdays(cal, '2013-08-13', '2017-10-02'), 1041)
    expect_equal(bizdays(cal, '2013-08-13', '2018-01-02'), 1102)
    expect_equal(bizdays(cal, '2013-08-13', '2018-04-02'), 1163)
    expect_equal(bizdays(cal, '2013-08-13', '2018-07-02'), 1226)
    expect_equal(bizdays(cal, '2013-08-13', '2018-10-01'), 1290)
    expect_equal(bizdays(cal, '2013-08-13', '2019-01-02'), 1352)
    expect_equal(bizdays(cal, '2013-08-13', '2019-04-01'), 1413)
    expect_equal(bizdays(cal, '2013-08-13', '2019-07-01'), 1475)
    expect_equal(bizdays(cal, '2013-08-13', '2019-10-01'), 1541)
    expect_equal(bizdays(cal, '2013-08-13', '2020-01-02'), 1605)
    expect_equal(bizdays(cal, '2013-08-13', '2020-04-01'), 1667)
    expect_equal(bizdays(cal, '2013-08-13', '2020-07-01'), 1728)
    expect_equal(bizdays(cal, '2013-08-13', '2020-10-01'), 1793)
    expect_equal(bizdays(cal, '2013-08-13', '2021-01-04'), 1856)
    expect_equal(bizdays(cal, '2013-08-13', '2021-07-01'), 1979)
    expect_equal(bizdays(cal, '2013-08-13', '2022-01-03'), 2107)
    expect_equal(bizdays(cal, '2013-08-13', '2022-07-01'), 2231)
    expect_equal(bizdays(cal, '2013-08-13', '2023-01-02'), 2358)
    expect_equal(bizdays(cal, '2013-08-13', '2024-01-02'), 2607)
    expect_equal(bizdays(cal, '2013-08-13', '2025-01-02'), 2861)
})

test_that('it should return the business days for a set of dates', {
    dates.from <- seq(as.Date('2013-01-01'), as.Date('2013-01-05'), by='day')
    dates.to <- dates.from + 5
    expect_equal(bizdays(cal, dates.from, dates.to), c(2, 3, 3, 3, 3))
})

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
    expect_equal(offset(cal, '2013-01-02', 1), as.Date('2013-01-03'))
    expect_equal(offset(cal, '2013-01-02', 3), as.Date('2013-01-07'))
    expect_equal(offset(cal, '2013-01-02', 0), as.Date('2013-01-02'))
    expect_equal(offset(cal, '2013-01-01', 0), as.Date('2013-01-02'))
    expect_equal(offset(cal, '2013-01-01', -1), as.Date('2012-12-28'), label=offset(cal, '2013-01-01', -1))
    dates <- c(as.Date('2013-01-01'), as.Date('2013-01-02'))
    expect_equal(offset(cal, dates, 1), c(as.Date('2013-01-03'), as.Date('2013-01-03')))
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


