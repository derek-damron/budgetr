context("plot_budget - Output")

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
                          , initial = 1000
                          , start = as.Date("2016-01-01")
                          , end = as.Date("2016-03-31")
                          )

test_that("Check", {
  expect_silent(plot.budget(my_budget))
  dev.off()
})
