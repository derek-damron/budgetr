context("print_budget")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = 1
                       , recurring = TRUE
                       )
rent <- create_item( name = "Rent"
                   , amount = -500
                   , day = 5
                   , recurring = TRUE
                   )
car <- create_item( name = "Car"
                  , amount = -200
                  , day = "last"
                  , recurring = TRUE
                  )
my_schedule <- create_schedule(paycheck, rent, car)
my_budget <- create_budget( my_schedule
                          , start=as.Date("2016-01-02")
                          , end=as.Date("2016-01-02") + 30
                          , initial=1000
                          )

test_that("Check", {
  expect_output(print(my_budget),
               "budget (budgetr)", fixed=TRUE)
  expect_output(print(my_budget),
               "date[ ]+name[ ]+balance")
  expect_output(print(my_budget),
               "2016-01-02[ ]+Initial Amount[ ]+1000")
  expect_output(print(my_budget),
               "+2016-01-05[ ]+Rent[ ]+500")
  expect_output(print(my_budget),
               "+2016-01-31[ ]+Car[ ]+300")
  expect_output(print(my_budget),
               "+2016-02-01[ ]+Paycheck[ ]+1300")
})
