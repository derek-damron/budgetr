context('get_items - Arguments')

test_that("Check - Errors", {
  expect_error(get_items("z"),
               "There are no objects in your current environment matching the pattern 'z'")
  x <- 1
  expect_error(get_items("x"),
               "There are no items in your current environment matching the pattern 'x'")
})
