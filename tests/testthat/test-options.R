context("bizdays.options")

test_that("new_defaults gets, sets, merges and restores values", {
  opts <- new_defaults(list(a = 1, b = 2))
  expect_equal(opts$get(), list(a = 1, b = 2))
  expect_equal(opts$get("a"), 1)
  expect_equal(opts$get("a", drop = FALSE), list(a = 1))
  expect_equal(opts$get(c("a", "b")), list(a = 1, b = 2))

  expect_null(opts$set())
  expect_null(opts$set(list()))
  opts$set(a = 10)
  expect_equal(opts$get("a"), 10)
  opts$set(list(b = 20, c = 30))
  expect_equal(opts$get(), list(a = 10, b = 20, c = 30))
  expect_equal(opts$get(default = TRUE), list(a = 1, b = 2))
  expect_equal(opts$merge(list(a = 0))$a, 0)
  expect_equal(opts$get("a"), 10) # merge does not change the stored values

  opts$restore()
  expect_equal(opts$get(), list(a = 1, b = 2))
})

test_that("default.calendar option is used by the functions", {
  old <- bizdays.options$get("default.calendar")
  on.exit(bizdays.options$set(default.calendar = old))
  bizdays.options$set(default.calendar = "weekends")
  expect_equal(bizdays("2024-01-05", "2024-01-08"), 1)
  expect_equal(weekdays(), c("saturday", "sunday"))
  expect_equal(holidays(), as.Date(integer(0)))
})
