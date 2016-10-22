context("print.budget")

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
groceries <- create_item( name = "Groceries"
                        , amount = -100
                        , day ="2016-01-03"
                        , recurring = "2 weeks"
                        )
my_schedule <- create_schedule(paycheck, rent, groceries)
my_budget <- create_budget( my_schedule
                          , start=as.Date("2016-01-01")
                          , end=as.Date("2016-01-01") + months(1)
                          , initial=1000
                          )

test_that("Check", {
    expect_output(print(my_budget),
               "budget (budgetr)", fixed=TRUE)
    expect_output(print(my_budget),
               "date[ ]+name[ ]+amount[ ]+balance")
    expect_output(print(my_budget),
               "2016-01-01[ ]+Initial Amount[ ]+1000[ ]+1000")
    expect_output(print(my_budget),
               "2016-01-01[ ]+Paycheck[ ]+1000[ ]+2000")
    expect_output(print(my_budget),
               "2016-01-03[ ]+Groceries[ ]+-100[ ]+1900")
    expect_output(print(my_budget),
               "2016-01-05[ ]+Rent[ ]+-500[ ]+1400")
    expect_output(print(my_budget),
               "2016-01-17[ ]+Groceries[ ]+-100[ ]+1300")
    expect_output(print(my_budget),
               "2016-01-31[ ]+Groceries[ ]+-100[ ]+1200")
    expect_output(print(my_budget),
               "2016-02-01[ ]+Paycheck[ ]+1000[ ]+2200")
})
