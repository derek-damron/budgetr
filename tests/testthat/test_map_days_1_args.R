context("map_days - Arguments")

test_that("Check - no arguments", {
  expect_identical(map_days(),
                   list())
})

test_that("Check - year", {
  expect_error(map_days(month=2),
               "Please provide a year")
  expect_error(map_days("a"),
               "year must be a numeric value")
  expect_error(map_days(-100),
               "year must be a positive value")
  expect_error(map_days(12.5),
               "year must be an integer")
})

test_that("Check - month", {
  expect_error(map_days(2016),
               "Please provide a month")
  expect_error(map_days(2016, "a"),
               "month must be a numeric value")
  expect_error(map_days(2016, -1),
               "month must be a positive integer between 1 and 12")
  expect_error(map_days(2016, 13),
               "month must be a positive integer between 1 and 12")
})

test_that("Check - day_first", {
  expect_error(map_days(2016, 1),
               "Please provide a day_first")
  expect_error(map_days(2016, 1, "a"),
               "day_first must be a numeric value")
  expect_error(map_days(2016, 1, -1),
               "day_first must be a positive integer between 1 and 31")
  expect_error(map_days(2016, 1, 32),
               "day_first must be a positive integer between 1 and 31")
})

test_that("Check - day_last", {
  expect_error(map_days(2016, 1, 1),
               "Please provide a day_last")
  expect_error(map_days(2016, 1, 1, "a"),
               "day_last must be a numeric value")
  expect_error(map_days(2016, 1, 1, -1),
               "day_last must be a positive integer between 1 and 31")
  expect_error(map_days(2016, 1, 1, 32),
               "day_last must be a positive integer between 1 and 31")
  expect_error(map_days(2016, 1, 31, 1),
               "day_last must greater than or equal to day_first")
})

test_that("Check - day_last", {
  expect_error(map_days(2016, 1, 1, 31),
               "Please provide a schedule_day")
  expect_error(map_days(2016, 1, 1, 31, "first"),
               "t currently have a way to convert first to a numeric day")
})

