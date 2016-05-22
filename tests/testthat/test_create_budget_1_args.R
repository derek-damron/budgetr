context("create_budget - Arguments")

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
my_bills <- create_schedule(paycheck, rent, internet)

test_that("Check - schedule", {
  expect_error(create_budget(),
               "Please provide a schedule for your budget")
  expect_error(create_budget(1:2),
               "Only one schedule should be provided")
  expect_error(create_budget(1),
               "The object provided is not a schedule")
})

test_that("Check - start", {
  expect_error(create_budget(my_bills, 1:2),
               "start must be a single value")
  expect_error(create_budget(my_bills, 1),
               "start must be a Date")
})

test_that("Check - end", {
  expect_error(create_budget(my_bills, end=1:2),
               "end must be a single value")
  expect_error(create_budget(my_bills, end=1),
               "end must be a Date")
  expect_error(create_budget(my_bills, end=Sys.Date()-1),
               "end must be at least one day after start")
})

test_that("Check - initial", {
  expect_error(create_budget(my_bills, initial=1:2),
               "initial must be a single value")
  expect_error(create_budget(my_bills, initial="a"),
               "initial must be a numeric value")
})
