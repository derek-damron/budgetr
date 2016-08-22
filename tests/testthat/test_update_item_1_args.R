context("update_item - Arguments")

name <- "Rent"
amount <- -500
day <- 1
recurring <- TRUE
rent <- create_item(name, amount, day, recurring)

test_that("Check - item", {
  expect_error(update_item(),
               "Please provide a budget item to update")
  expect_error(update_item(item=1:2),
               "Only one budget item can be updated at a time")
  expect_error(update_item(item=1),
               "item must be a budget item")
})

test_that("Check - item args", {
  rent <- update_item(rent)
  expect_identical(rent$df,
                   data.frame( name = name
                             , amount = amount
                             , day = as.character(day)
                             , recurring = recurring
                             , stringsAsFactors = FALSE
                             ))
})
