context("print_budget")

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
my_budget <- create_budget( my_bills
                          , start=as.Date("2016-01-02")
                          , end=as.Date("2016-01-02") + 30
                          , initial=500
                          )

test_that("Check", {
  expect_output(print(my_budget),
               "budget")
  expect_output(print(my_budget),
               "day[ ]+date[ ]+name[ ]+amount[ ]+recurring[ ]+balance")
  expect_output(print(my_budget),
               "2[ ]+2016-01-02[ ]+Initial Amount[ ]+500[ ]+FALSE[ ]+500")
  expect_output(print(my_budget),
               "15[ ]+2016-01-15[ ]+Internet[ ]+-100[ ]+TRUE[ ]+400")
  expect_output(print(my_budget),
               "1[ ]+2016-02-01[ ]+Paycheck[ ]+1050[ ]+TRUE[ ]+1450")
  expect_output(print(my_budget),
               "1[ ]+2016-02-01[ ]+Rent[ ]+-800[ ]+TRUE[ ]+650")
})
