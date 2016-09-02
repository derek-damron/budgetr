context("last_day_of_month - Output")

years <- rep(2015:2016, each = 12)
months <- rep(1:12, times = 2)

test_that("Check", {
  output <- last_day_of_month(years, months)
  expect_identical(output,
                   c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
                     31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
})
