context("create_schedule - Arguments")

name <- "Paycheck"
amount <- 1000
day <- "2016-01-01"
recurring <- "monthly"
paycheck <- create_item(name, amount, day, recurring)

test_that("Check - items", {
  expect_error(create_schedule(),
               "Please provide at least one budget item")
  expect_error(create_schedule(paycheck, 1),
               "At least one of the objects provided isn't a budget item")
})
