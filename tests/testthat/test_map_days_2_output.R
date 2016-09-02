context("map_days - Output")

test_that("Check - numeric", {
  expect_is(map_days(2015, 1, 1, 1, as.character(1:31)),
            "numeric")
  expect_equal(map_days(2015, 1, 1, 1, as.character(1:31)),
               1:31)
})

test_that("Check - last", {
  expect_is(map_days(2015, 1, 1, 1:31, as.character(1:31)),
            "numeric")
  expect_equal(map_days(2015, 1, 1, 1:31, as.character(1:31)),
               1:31)
})
