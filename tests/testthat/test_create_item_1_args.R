context("create_item - Arguments")

name <- "Rent"
amount <- -800
day <- 1
recurring <- TRUE

rent <- create_item(name, amount, day, recurring)

test_that("Check - name", {
  expect_error(create_item(),
               "Please provide a name for the budget item")
  expect_error(create_item(name=1:2),
               "name must be a single value")
  expect_identical(rent$df,
                   data.frame( name = as.character(1)
                             , amount = amount
                             , day = as.character(day)
                             , recurring = recurring
                             , stringsAsFactors=FALSE
                             ))
})

test_that("Check - amount", {
  expect_error(create_item(name),
               "Please provide an amount for the budget item")
  expect_error(create_item(name, amount=1:2),
               "amount must be a single value")
  expect_error(create_item(name, amount="a"),
               "amount must be a numeric value")
})

test_that("Check - day", {
  expect_error(create_item(name, amount),
               "Please provide a day for the budget item")
  expect_error(create_item(name, amount, day=1:2),
               "day must be a single value")
})

test_that("Check - recurring", {
  expect_error(create_item(name, amount, day),
               "Please note whether the budget item is recurring")
  expect_error(create_item(name, amount, day, recurring=1:2),
               "recurring must be a single value")
  expect_error(create_item(name, amount, day, recurring="a"),
               "recurring must be TRUE or FALSE")
})
