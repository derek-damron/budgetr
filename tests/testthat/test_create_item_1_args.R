context("create_item - Arguments")

name <- "Paycheck"
amount <- 1000
day <- "2016-01-01"
recurring <- "monthly"

test_that("Check - name", {
  expect_error(create_item(),
               "Please provide a name for the budget item")
  expect_error(create_item(name=1:2),
               "name must be a single value")
  expect_equal(create_item(1, amount, day, recurring)$name
              , "1")
})

test_that("Check - amount", {
  expect_error(create_item(name),
               "Please provide an amount for the budget item")
  expect_error(create_item(name, amount=1:2),
               "amount must be a single value")
  expect_error(create_item(name, amount="a"),
               "Could not convert amount into a numeric value")
  expect_equal(create_item(name, "1000", day, recurring)$amount
              , 1000)
})

test_that("Check - day", {
  expect_error(create_item(name, amount),
               "Please provide a day for the budget item")
  expect_error(create_item(name, amount, day=1:2),
               "day must be a single value")
  expect_error(create_item(name, amount, day=1),
               "Could not convert day into a Date object using lubridate::ymd")
  expect_equal(create_item(name, amount, as.Date("2016-01-01"), recurring)$day
              , as.Date("2016-01-01"))
  expect_equal(create_item(name, amount, "2016-01-01", recurring)$day
              , as.Date("2016-01-01"))
  expect_equal(create_item(name, amount, "20160101", recurring)$day
              , as.Date("2016-01-01"))
  expect_equal(create_item(name, amount, 20160101, recurring)$day
              , as.Date("2016-01-01"))
})

test_that("Check - recurring", {
  expect_equal(create_item(name, amount, day)$recurring,
               FALSE)
  expect_error(create_item(name, amount, day, recurring=1:2),
               "recurring must be a single value")
  expect_error(create_item(name, amount, day, recurring=1),
               "recurring is not a recognized value, see Details in ?create_item for possible values",
               fixed = TRUE)
  expect_equal(create_item(name, amount, day, recurring)$recurring,
               "monthly")
})
