
source('calendar.R')

# data(holidaysANDIMA)
holidaysANDIMA <- as.Date(unlist(read.table('ANBIMA.cal')))
# holidaysANDIMA <- holidaysANDIMA[holidaysANDIMA >= '2013-01-01' & holidaysANDIMA <= '2026-01-01']
cal <- calendar(holidaysANDIMA)

context('test calendar - count bizdays')

test_that("business days counting", {
    busdays <- function(from, to) cal$bizdays(from, to)
    expect_that(busdays('2013-07-12', '2014-07-12'), equals(251))
    expect_that(busdays('2013-08-21', '2013-08-24'), equals(2))
    expect_that(busdays('2013-01-01', '2013-01-31'), equals(21))
    expect_that(busdays('2013-01-01', '2014-01-01'), equals(252))
    expect_that(busdays('2014-01-01', '2015-01-01'), equals(252))
    expect_that(busdays('2014-10-10', '2015-02-11'), equals(86))
    expect_that(busdays('2013-08-13', '2013-09-02'), equals(14))
    expect_that(busdays('2013-08-13', '2013-10-01'), equals(35))
    expect_that(busdays('2013-08-13', '2013-11-01'), equals(58))
    expect_that(busdays('2013-08-13', '2013-12-02'), equals(78))
    expect_that(busdays('2013-08-13', '2014-01-02'), equals(99))
    expect_that(busdays('2013-08-13', '2014-04-01'), equals(160))
    expect_that(busdays('2013-08-13', '2014-07-01'), equals(221))
    expect_that(busdays('2013-08-13', '2014-10-01'), equals(287))
    expect_that(busdays('2013-08-13', '2015-01-02'), equals(352))
    expect_that(busdays('2013-08-13', '2015-04-01'), equals(413))
    expect_that(busdays('2013-08-13', '2015-07-01'), equals(474))
    expect_that(busdays('2013-08-13', '2015-10-01'), equals(539))
    expect_that(busdays('2013-08-13', '2016-01-04'), equals(602))
    expect_that(busdays('2013-08-13', '2016-04-01'), equals(663))
    expect_that(busdays('2013-08-13', '2016-07-01'), equals(726))
    expect_that(busdays('2013-08-13', '2016-10-03'), equals(791))
    expect_that(busdays('2013-08-13', '2017-01-02'), equals(853))
    expect_that(busdays('2013-08-13', '2017-04-03'), equals(916))
    expect_that(busdays('2013-08-13', '2017-07-03'), equals(977))
    expect_that(busdays('2013-08-13', '2017-10-02'), equals(1041))
    expect_that(busdays('2013-08-13', '2018-01-02'), equals(1102))
    expect_that(busdays('2013-08-13', '2018-04-02'), equals(1163))
    expect_that(busdays('2013-08-13', '2018-07-02'), equals(1226))
    expect_that(busdays('2013-08-13', '2018-10-01'), equals(1290))
    expect_that(busdays('2013-08-13', '2019-01-02'), equals(1352))
    expect_that(busdays('2013-08-13', '2019-04-01'), equals(1413))
    expect_that(busdays('2013-08-13', '2019-07-01'), equals(1475))
    expect_that(busdays('2013-08-13', '2019-10-01'), equals(1541))
    expect_that(busdays('2013-08-13', '2020-01-02'), equals(1605))
    expect_that(busdays('2013-08-13', '2020-04-01'), equals(1667))
    expect_that(busdays('2013-08-13', '2020-07-01'), equals(1728))
    expect_that(busdays('2013-08-13', '2020-10-01'), equals(1793))
    expect_that(busdays('2013-08-13', '2021-01-04'), equals(1856))
    expect_that(busdays('2013-08-13', '2021-07-01'), equals(1979))
    expect_that(busdays('2013-08-13', '2022-01-03'), equals(2107))
    expect_that(busdays('2013-08-13', '2022-07-01'), equals(2231))
    expect_that(busdays('2013-08-13', '2023-01-02'), equals(2358))
    expect_that(busdays('2013-08-13', '2024-01-02'), equals(2607))
    expect_that(busdays('2013-08-13', '2025-01-02'), equals(2861))
})

test_that("is business day", {
    expect_false(cal$is.bizday(as.Date('2013-01-01')))
    expect_true(cal$is.bizday(as.Date('2013-01-02')))
})

context('test calendar - next and previous bizdays')

test_that("next business day", {
    date <- as.character(cal$adjust.next('2013-01-01'))
    expect_that(date, equals('2013-01-02'))
})

test_that("previous business day", {
    date <- as.character(cal$adjust.previous('2013-02-02'))
    expect_that(date, equals('2013-02-01'))
})

context('test calendar - sequence of bizdays')

test_that("sequence of bizdays", {
	s <- c("2013-01-02","2013-01-03","2013-01-04","2013-01-07","2013-01-08",
	"2013-01-09","2013-01-10")
    expect_true(all( cal$seq('2013-01-01', '2013-01-10') == s ))
})
