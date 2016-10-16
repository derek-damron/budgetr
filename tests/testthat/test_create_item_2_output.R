context('create_item - Output')

name <- "Paycheck"
amount <- 1000
day <- "2016-01-01"
recurring <- "monthly"
paycheck <- create_item(name, amount, day, recurring)

test_that("Check - Output", {
  expect_true(is.item(paycheck))
  expect_true(is.list(paycheck))
  expect_identical(paycheck$name, "Paycheck")
  expect_identical(paycheck$amount, 1000)
  expect_identical(paycheck$day, as.Date("2016-01-01"))
  expect_identical(paycheck$recurring, "monthly")
})
