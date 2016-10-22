context("print.schedule")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = "2016-01-01"
                       , recurring = "monthly"
                       )
rent <- create_item( name = "Rent"
                   , amount = -500
                   , day = "2016-01-05"
                   , recurring = "monthly"
                   )
my_schedule <- create_schedule(paycheck, rent)

test_that("Check", {
  expect_output(print(my_schedule),
               "schedule (budgetr)", fixed=TRUE)
  expect_output(print(my_schedule),
               "name[ ]+amount[ ]+day[ ]+recurring")
  expect_output(print(my_schedule),
               "Paycheck[ ]+1000[ ]+2016-01-01[ ]+monthly")
  expect_output(print(my_schedule),
               "Rent[ ]+-500[ ]+2016-01-05[ ]+monthly")
})
