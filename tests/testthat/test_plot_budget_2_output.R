context("plot_budget")

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
  expect_silent(plot.budget(my_budget))
})
