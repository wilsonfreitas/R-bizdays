
context("getbizdays")

test_that("getbizdays works with years", {
  dc <- getbizdays(2022, "actual")
  expect_equal(dc, 365)

  dc <- getbizdays(2022:2024, "actual")
  expect_equal(dc, c(365, 365, 366))
})

test_that("getbizdays works with year-month", {
  dc <- getbizdays("2022-12", "actual")
  expect_equal(dc, 31)

  dc <- getbizdays(paste0(2022, "-", 10:12), "actual")
  expect_equal(dc, c(31, 30, 31))
})

test_that("getbizdays works with dates", {
  dc <- getbizdays("2022-12-02", "actual")
  expect_equal(dc, 31)

  dc <- getbizdays(as.Date("2022-12-02"), "actual")
  expect_equal(dc, 31)

  dts <- seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "months")
  dc <- getbizdays(dts, "actual")
  expect_equal(dc, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
})