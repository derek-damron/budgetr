context('get_items - Output')

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = as.Date("2016-01-01")
                       , recurring = "monthly"
                       )
rent <- create_item( name = "Rent"
                   , amount = -500
                   , day = as.Date("2016-01-05")
                   , recurring = "monthly"
                   )
internet <- create_item( name = "Internet"
                       , amount = -100
                       , day = as.Date("2016-01-15")
                       , recurring = "monthly"
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
