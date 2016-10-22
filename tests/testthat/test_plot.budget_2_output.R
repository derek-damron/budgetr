context("plot.budget - Output")

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
  expect_silent(plot.budget(my_budget))
  dev.off()
})
