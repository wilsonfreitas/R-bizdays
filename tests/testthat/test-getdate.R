test_that("getdate", {
  expect_equal(getdate("10th wed", 2018, "actual"), as.Date("2018-03-07"))
  dts <- getdate("last bizday", 2010:2018, "Brazil/ANBIMA")
  dts_1 <- as.Date(c(
    "2010-12-31", "2011-12-30", "2012-12-31", "2013-12-31", "2014-12-31",
    "2015-12-31", "2016-12-30", "2017-12-29", "2018-12-31"
  ))
  expect_equal(dts, dts_1)
  dts <- seq(as.Date("2018-01-01"), as.Date("2018-12-01"), "month")
  dts_1 <- getdate("first bizday", format(dts, "%Y-%m"), "Brazil/ANBIMA")
  dts_2 <- as.Date(c(
    "2018-01-02", "2018-02-01", "2018-03-01", "2018-04-02", "2018-05-02",
    "2018-06-01", "2018-07-02", "2018-08-01", "2018-09-03", "2018-10-01",
    "2018-11-01", "2018-12-03"
  ))
  expect_equal(dts_1, dts_2)
})

test_that("getdate by_day", {
  x <- getdate("next day", Sys.Date(), "actual")
  expect_equal(x, Sys.Date() + 1)
  x <- getdate("last day", Sys.Date(), "actual")
  expect_equal(x, Sys.Date() - 1)
  x <- getdate("next bizday", "2022-06-18", "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-20"))
  x <- getdate("last bizday", "2022-06-19", "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-17"))

  x <- getdate("2nd wed", as.Date("2022-06-20"), "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-29"))
  x <- getdate("next wed", as.Date("2022-06-20"), "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-22"))
  x <- getdate("last wed", as.Date("2022-06-20"), "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-15"))
  x <- getdate("next mon", as.Date("2022-06-20"), "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-27"))
  x <- getdate("last mon", as.Date("2022-06-20"), "Brazil/ANBIMA")
  expect_equal(x, as.Date("2022-06-13"))
})