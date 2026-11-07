
context("bizdays with default calendar")

test_that("bizdays using default calendar", {
  # The default Calendar has no holidays and weekends as non-working days
  # saturday and sunday
  expect_equal(bizdays("2013-01-02", "2013-01-03"), 1)
  expect_equal(bizdays(as.Date("2013-01-02"), "2013-01-03"), 1)
  expect_equal(bizdays(as.Date("2013-01-02"), as.Date("2013-01-03")), 1)
  expect_error(bizdays("2013-31-02", "2013-01-03"))
})

test_that("it should pass Date sequences to bizdays", {
  expect_equal(bizdays(c("2013-01-02", "2013-01-03"), "2013-01-03"), c(1, 0))
})

test_that("it should bizdays a set of dates", {
  dates.from <- seq(as.Date("2013-01-01"), as.Date("2013-01-05"), by = "day")
  dates.to <- dates.from + 5
  expect_equal(bizdays(dates.from, dates.to), c(5, 5, 5, 5, 5))
  expect_equal(bizdays("2013-01-02", dates.to), c(4, 5, 6, 7, 8))
  expect_equal(bizdays(dates.from, "2013-01-08"), c(7, 6, 5, 4, 3))
  expect_error(
    bizdays(
      c("2013-01-08", "2013-01-08", "2013-01-08"),
      c("2013-01-08", "2013-01-08")
    ),
    "from's length must be multiple of to's length and vice-versa."
  )
})

test_that("it should return negative bizdays", {
  cal <- calendars()[["Brazil/ANBIMA"]]
  cal <- Calendar_(cal$holidays, weekdays = c("saturday", "sunday"))
  expect_equal(bizdays("2014-07-12", "2013-07-12", cal), -251)
  expect_equal(
    bizdays(
      c("2013-08-21", "2013-01-31", "2013-01-01"),
      c("2013-08-24", "2013-01-01", "2014-01-01"), cal
    ),
    c(2, -21, 252)
  )
  expect_equal(
    bizdays(
      c("2013-08-21", "2013-01-31", "2013-01-01"),
      c(NA, "2013-01-01", "2014-01-01"), cal
    ),
    c(NA, -21, 252)
  )
  cal <- create.calendar(
    name = "test_neg_bizdays",
    weekdays = c("saturday", "sunday"),
    adjust.from = adjust.none, adjust.to = adjust.none
  )
  expect_equal(
    bizdays(Sys.Date(), Sys.Date() + c(2, -1, 1, 1), "actual"),
    c(2, -1, 1, 1)
  )
})

test_that("it should check consistency in bizdays", {
  cal <- create.calendar(
    name = "test_consistency",
    weekdays = c("saturday", "sunday"),
    adjust.from = adjust.none, adjust.to = adjust.none
  )
  expect_equal(bizdays("2013-06-22", "2013-06-23", cal), 0)
  expect_equal(bizdays("2013-06-23", "2013-06-22", cal), 0)
  
  hol <- c("2024-01-01", "2024-03-29", "2024-04-01", "2024-05-06", "2024-05-27", 
           "2024-08-26", "2024-12-25", "2024-12-26")
  cal <- bizdays::create.calendar(name = "nursery_calendar",
                                  holidays = hol,
                                  weekdays = c("monday", "tuesday", "wednesday",
                                               "saturday", "sunday"),
                                  start.date = as.Date("2024-01-01"),
                                  end.date = as.Date("2024-12-31"),
                                  financial = FALSE)
  expect_equal(bizdays("2024-12-23", "2024-12-29", cal), 1)
  expect_equal(bizdays("2024-12-29", "2024-12-23", cal), -1)
  expect_true(is.bizday("2024-12-27", cal))
})

test_that("it should compute bizdays using double index approach", {
  create.calendar(
    name = "example1", weekdays = c("saturday", "sunday"),
    start.date = "2017-01-24", end.date = "2017-01-30",
    holidays = "2017-01-25"
  )
  expect_equal(
    bizdays("2017-01-24", "2017-01-25", "example1"),
    bizdays("2017-01-25", "2017-01-26", "example1")
  )
})

test_that("is should compute bizdays for one single day", {
  create.calendar("actual-no-fin", financial = FALSE)

  expect_equal(bizdays("2021-12-30", "2021-12-30", "actual"), 0)
  expect_equal(bizdays("2021-12-30", "2021-12-30", "actual-no-fin"), 1)
})

test_that("is should compute negative bizdays for non finacial calendars", {
  create.calendar("actual-no-fin", financial = FALSE)

  expect_equal(
    -bizdays("2014-07-12", "2013-07-12", "actual-no-fin"),
    bizdays("2013-07-12", "2014-07-12", "actual-no-fin")
  )
})

context("handling NA values")

test_that("it should bizdays NA values", {
  expect_equal(
    bizdays("2013-01-01", c("2013-12-31", "2014-12-31", NA)),
    c(364, 729, NA)
  )
  expect_equal(
    adjust.next(c("2013-12-31", "2014-12-31", NA)),
    as.Date(c("2013-12-31", "2014-12-31", NA))
  )
  expect_equal(
    adjust.previous(c("2013-12-31", "2014-12-31", NA)),
    as.Date(c("2013-12-31", "2014-12-31", NA))
  )
  expect_equal(is.bizday(c("2013-12-31", "2014-12-31", NA)), c(TRUE, TRUE, NA))
})

test_that("it should bizdays all NA values", {
  expect_equal(bizdays("2013-01-01", NA), NA)
  expect_equal(bizdays(c("2013-01-01", "2013-02-01"), NA), c(NA, NA))
  expect_equal(bizdays("2013-01-01", c(NA, NA)), c(NA, NA))
})

context("bizdays and current days equivalence")

test_that("it should compute the business days equivalent to current days", {
  cal <- calendars()[["Brazil/ANBIMA"]]
  expect_equal(bizdayse("2013-08-21", 3, cal), 2)
})
test_that("POSIXct dates use their own time zone (#109, #111)", {
  sat_nz <- as.POSIXct("2025-01-04", tz = "Pacific/Auckland")
  expect_equal(following(sat_nz, "weekends"), as.Date("2025-01-06"))
  expect_true(!is.bizday(sat_nz, "weekends"))
  late_sp <- as.POSIXct("2024-03-05 23:57:07", tz = "America/Sao_Paulo")
  expect_equal(offset(late_sp, -1, "Brazil/ANBIMA"), as.Date("2024-03-04"))
  expect_equal(bizdays(late_sp, "2024-03-06", "Brazil/ANBIMA"), 1)
})

test_that("bizdays is 0 when no business day is in the range (#114)", {
  cal <- "Brazil/ANBIMA"
  expect_equal(bizdays("2024-01-06", "2024-01-07", cal), 0) # Sat -> Sun
  expect_equal(bizdays("2024-02-10", "2024-02-13", cal), 0) # Carnival
  expect_equal(bizdays("2024-01-07", "2024-01-06", cal), 0) # reversed
  expect_equal(bizdayse("2024-01-06", 1, cal), 0)
  # other ranges in the same vector are not changed
  expect_equal(
    bizdays(c("2024-01-06", "2024-01-05", NA), c("2024-01-07", "2024-01-08", "2024-01-08"), cal),
    c(0, 1, NA)
  )
  for (fin in c(TRUE, FALSE)) {
    create.calendar("test-114", weekdays = c("saturday", "sunday"),
                    adjust.from = adjust.none, adjust.to = adjust.previous,
                    financial = fin)
    expect_equal(bizdays("2024-01-06", "2024-01-07", "test-114"), 0)
  }
})
