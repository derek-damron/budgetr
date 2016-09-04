context("map_days - Output")

test_that("Check - numeric", {
  expect_is(map_days(2015, 1, 1, 1, as.character(1:31)),
            "numeric")
  expect_equal(map_days(2015, 1, 1, 1, as.character(1:31)),
               1:31)
})

test_that("Check - date", {
  # No matches
  expect_is(map_days(2015, 2, 1, 1, as.Date("2016-01-01")),
            "numeric")
  expect_equal(map_days(2015, 2, 1, 1, as.Date("2016-01-01")),
               NA_real_)

  # Year mismatch
  expect_is(map_days(2015, 1, 1, 1, as.Date("2016-01-01")),
            "numeric")
  expect_equal(map_days(2015, 1, 1, 1, as.Date("2016-01-01")),
               NA_real_)

  # Month mismatch
  expect_is(map_days(2016, 2, 1, 1, as.Date("2016-01-01")),
            "numeric")
  expect_equal(map_days(2016, 2, 1, 1, as.Date("2016-01-01")),
               NA_real_)

  # Full match
  expect_is(map_days(2016, 1, 1, 1, as.Date("2016-01-01")),
            "numeric")
  expect_equal(map_days(2016, 1, 1, 1, as.Date("2016-01-01")),
               1)
})

test_that("Check - last", {
  expect_is(map_days(2015, 1, 1, 1:31, as.character(1:31)),
            "numeric")
  expect_equal(map_days(2015, 1, 1, 1:31, as.character(1:31)),
               1:31)
})
