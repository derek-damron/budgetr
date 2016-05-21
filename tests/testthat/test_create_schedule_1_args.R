context("create_schedule - Arguments")

name <- "Rent"
amount <- -800
day <- 1
recurring <- TRUE
rent <- create_item(name, amount, day, recurring)

test_that("Check - items", {
  expect_error(create_schedule(),
               "Please provide at least one budget item")
  expect_error(create_schedule(rent, 1),
               "At least one of the objects provided isn't a budget item")
})
