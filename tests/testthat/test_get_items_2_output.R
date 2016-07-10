context('get_items - Output')

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
expected <- list( internet = internet
                , paycheck = paycheck
                , rent = rent
                )
actual <- get_items()

test_that("Check - No pattern", {
  expect_identical(actual, expected)
})

expected <- list(paycheck = paycheck)
actual <- get_items("a")

test_that("Check - Pattern", {
  expect_identical(actual, expected)
})
