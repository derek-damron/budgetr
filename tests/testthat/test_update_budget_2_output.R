context('update_budget - Output')

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
                          , start = as.Date("2015-12-15")
                          , end = as.Date("2016-03-02")
                          )

test_that("Check - nothing", {
  my_budget <- update_budget(my_budget)
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( date = as.Date(c("2015-12-15", "2015-12-31", "2016-01-01", "2016-01-05", "2016-01-31", "2016-02-01", "2016-02-05", "2016-02-29", "2016-03-01"))
                             , name = c("Initial Amount", "Car", "Paycheck", "Rent", "Car", "Paycheck", "Rent", "Car", "Paycheck")
                             , amount = c(1000, -200, 1000, -500, -200, 1000, -500, -200, 1000)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(1000, 800, 1800, 1300, 1100, 2100, 1600, 1400, 2400)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - start", {
  my_budget <- update_budget(my_budget, start=as.Date("2016-02-15"))
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( date = as.Date(c("2016-02-15", "2016-02-29", "2016-03-01"))
                             , name = c("Initial Amount", "Car", "Paycheck")
                             , amount = c(1000, -200, 1000)
                             , recurring = c(FALSE, TRUE, TRUE)
                             , balance = c(1000, 800, 1800)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - end", {
  my_budget <- update_budget(my_budget, end=as.Date("2016-02-15"))
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( date = as.Date(c("2015-12-15", "2015-12-31", "2016-01-01", "2016-01-05", "2016-01-31", "2016-02-01", "2016-02-05"))
                             , name = c("Initial Amount", "Car", "Paycheck", "Rent", "Car", "Paycheck", "Rent")
                             , amount = c(1000, -200, 1000, -500, -200, 1000, -500)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(1000, 800, 1800, 1300, 1100, 2100, 1600)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - initial", {
  my_budget <- update_budget(my_budget, initial=0)
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( date = as.Date(c("2015-12-15", "2015-12-31", "2016-01-01", "2016-01-05", "2016-01-31", "2016-02-01", "2016-02-05", "2016-02-29", "2016-03-01"))
                             , name = c("Initial Amount", "Car", "Paycheck", "Rent", "Car", "Paycheck", "Rent", "Car", "Paycheck")
                             , amount = c(0, -200, 1000, -500, -200, 1000, -500, -200, 1000)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(0, -200, 800, 300, 100, 1100, 600, 400, 1400)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - all", {
  my_budget <- update_budget( my_budget
                            , start=as.Date("2016-03-01")
                            , end=as.Date("2016-05-02")
                            , initial=0
                            )
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( date = as.Date(c("2016-03-01", "2016-03-01", "2016-03-05", "2016-03-31", "2016-04-01", "2016-04-05", "2016-04-30", "2016-05-01"))
                             , name = c("Initial Amount", "Paycheck", "Rent", "Car", "Paycheck", "Rent", "Car", "Paycheck")
                             , amount = c(0, 1000, -500, -200, 1000, -500, -200, 1000)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(0, 1000, 500, 300, 1300, 800, 600, 1600)
                             , stringsAsFactors = FALSE
                             ))
})
