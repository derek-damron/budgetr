context("update_budget - Arguments")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = 1
                       , recurring = TRUE
                       )
rent <- create_item( name = "Rent"
                   , amount = -500
                   , day = 5
                   , recurring = TRUE
                   )
my_schedule <- create_schedule(paycheck, rent)
my_budget <- create_budget(my_schedule, initial=1000, start=as.Date("2016-01-01"))

test_that("Check - budget", {
  expect_error(update_budget(),
               "Please provide a budget to update")
  expect_error(update_budget(1),
               "budget must be a budget")
})

test_that("Check - str", {
  expect_identical(my_budget$args$start,
                   as.Date("2016-01-01"))
  expect_identical(my_budget$args$end,
                   as.Date("2016-03-31"))
  expect_identical(my_budget$args$initial,
                   1000)
})
