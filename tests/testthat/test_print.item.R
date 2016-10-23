context("print.item")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = as.Date("2016-01-01")
                       , recurring = "monthly"
                       )

test_that("Check", {
  expect_output(print(paycheck),
               "item (budgetr)", fixed=TRUE)
  expect_output(print(paycheck),
               "name[ ]+amount[ ]+day[ ]+recurring")
  expect_output(print(paycheck),
               "Paycheck[ ]+1000[ ]+2016-01-01[ ]+monthly")
})
