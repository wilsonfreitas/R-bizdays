context("Nager.Date calendars")

test_that("nager_holidays keeps national and subdivision holidays", {
  x <- jsonlite::fromJSON(test_path("nager_BR_2024.json"))
  h <- nager_holidays(x)
  expect_false(as.Date("2024-07-09") %in% h) # Sao Paulo state holiday
  expect_true(all(as.Date(c("2024-02-12", "2024-11-20")) %in% h))
  # same holidays as ANBIMA, except Easter Sunday (a Sunday anyway)
  anbima <- holidays("Brazil/ANBIMA")
  anbima <- anbima[format(anbima, "%Y") == "2024"]
  expect_equal(setdiff(h, anbima), as.Date("2024-03-31"))
  expect_equal(setdiff(anbima, h), as.Date(character(0)))
  expect_true(as.Date("2024-07-09") %in% nager_holidays(x, "BR-SP"))
  expect_equal(nager_holidays(list()), as.Date(character(0)))
})

test_that("load_nager_calendar validates its arguments", {
  expect_error(load_nager_calendar("BRA", 2024), "2-letter")
  expect_error(load_nager_calendar("../x", 2024), "2-letter")
  expect_error(load_nager_calendar("BR", NA), "years")
  expect_error(load_nager_calendar("BR", "2024"), "years")
})

test_that("load_nager_calendar downloads holidays", {
  skip_on_cran()
  skip_if_offline("date.nager.at")
  cal <- expect_message(load_nager_calendar("br", 2023:2024), "Nager/BR")
  expect_equal(cal$name, "Nager/BR")
  expect_equal(cal$start.date, as.Date("2023-01-01"))
  expect_equal(cal$end.date, as.Date("2024-12-31"))
  expect_equal(bizdays("2024-02-09", "2024-02-14", "Nager/BR"), 1) # Carnival
  expect_error(load_nager_calendar("XX", 2024), "Could not get the holidays")
})
