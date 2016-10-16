context("update_item - Arguments")

name <- "Paycheck"
amount <- 1000
day <- "2016-01-01"
recurring <- "monthly"
paycheck <- create_item(name, amount, day, recurring)

test_that("Check - item", {
  expect_error(update_item(),
               "Please provide a budget item to update")
  expect_error(update_item(item=1),
               "item must be a budget item")
})
