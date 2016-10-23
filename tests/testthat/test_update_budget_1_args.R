context("update_budget - Arguments")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = "2016-01-01"
                       , recurring = "monthly"
                       )
rent <- create_item( name = "Rent"
                   , amount = -500
                   , day = "2016-01-05"
                   , recurring = "monthly"
                   )
my_schedule <- create_schedule(paycheck, rent)
my_budget <- create_budget( my_schedule
                          , start = "2015-12-15"
                          , end = "2016-03-02"
                          , initial = 1000
                          )

test_that("Check - budget", {
  expect_error(update_budget(),
               "Please provide a budget to update")
  expect_error(update_budget(1),
               "budget must be a budget")
})

test_that("Check - str", {
  expect_identical(my_budget$args$start,
                   as.Date("2015-12-15"))
  expect_identical(my_budget$args$end,
                   as.Date("2016-03-02"))
  expect_identical(my_budget$args$initial,
                   1000)
})
