context("last_day_of_month - Arguments")

test_that("Check - no arguments", {
  expect_identical(last_day_of_month(),
                   list())
})

test_that("Check - year", {
  expect_error(last_day_of_month(month=2),
               "Please provide a year")
  expect_error(last_day_of_month("a"),
               "year must be a numeric value")
  expect_error(last_day_of_month(-100),
               "year must be a positive value")
  expect_error(last_day_of_month(12.5),
               "year must be an integer")
})

test_that("Check - month", {
  expect_error(last_day_of_month(2016),
               "Please provide a month")
  expect_error(last_day_of_month(2016, "a"),
               "month must be a numeric value")
  expect_error(last_day_of_month(2016, -1),
               "month must be a positive integer between 1 and 12")
  expect_error(last_day_of_month(2016, 13),
               "month must be a positive integer between 1 and 12")
})
