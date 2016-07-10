context('get_items')

test_that("Check - Errors", {
  expect_error(get_items("z"),
               "There are no objects in your current environment matching the pattern 'z'")
  x <- 1
  expect_error(get_items("x"),
               "There are no items in your current environment matching the pattern 'x'")
})

test_that("Check - No pattern", {
  paycheck <- create_item( name = "Paycheck"
                         , amount = 1050
                         , day = 1
                         , recurring = TRUE
                         )
  rent <- create_item( name = "Rent"
                     , amount = -800
                     , day = 1
                     , recurring = TRUE
                     )
  internet <- create_item( name = "Internet"
                         , amount = -100
                         , day = 15
                         , recurring = TRUE
                         )

  actual <- get_items()
  expected <- list( internet = internet
                  , paycheck = paycheck
                  , rent = rent
                  )
  expect_identical(actual, expected)
})
