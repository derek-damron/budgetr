context('create_budget - Output')

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
my_budget

test_that("Check - Output", {
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_equivalent(my_budget$df,
                    data.frame( day = c("2", "15", "1", "1")
                              , date = as.Date(c("2016-01-02", "2016-01-15", "2016-02-01", "2016-02-01"))
                              , name = c("Initial Amount", "Internet", "Paycheck", "Rent")
                              , amount = c(500, -100, 1050, -800)
                              , recurring = c(FALSE, TRUE, TRUE, TRUE)
                              , balance = c(500, 400, 1450, 650)
                              , stringsAsFactors=FALSE
                              ))
})
