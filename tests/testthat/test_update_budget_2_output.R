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
my_schedule <- create_schedule(paycheck, rent)
my_budget <- create_budget(my_schedule, initial=1000, start=as.Date("2016-01-01"))

test_that("Check - nothing", {
  my_budget <- update_budget(my_budget)
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( day = c("1", "1", "5", "1", "5", "1", "5")
                             , date = as.Date(c("2016-01-01", "2016-01-01", "2016-01-05", "2016-02-01", "2016-02-05", "2016-03-01", "2016-03-05"))
                             , name = c("Initial Amount", "Paycheck", "Rent", "Paycheck", "Rent", "Paycheck", "Rent")
                             , amount = c(1000, 1000, -500, 1000, -500, 1000, -500)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(1000, 2000, 1500, 2500, 2000, 3000, 2500)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - start", {
  my_budget <- update_budget(my_budget, start=as.Date("2016-03-01"))
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( day = c("1", "1", "5", "1", "5", "1", "5")
                             , date = as.Date(c("2016-03-01", "2016-03-01", "2016-03-05", "2016-04-01", "2016-04-05", "2016-05-01", "2016-05-05"))
                             , name = c("Initial Amount", "Paycheck", "Rent", "Paycheck", "Rent", "Paycheck", "Rent")
                             , amount = c(1000, 1000, -500, 1000, -500, 1000, -500)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(1000, 2000, 1500, 2500, 2000, 3000, 2500)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - end", {
  my_budget <- update_budget(my_budget, end=as.Date("2016-04-30"))
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( day = c("1", "1", "5", "1", "5", "1", "5", "1", "5")
                             , date = as.Date(c("2016-01-01", "2016-01-01", "2016-01-05", "2016-02-01", "2016-02-05", "2016-03-01", "2016-03-05", "2016-04-01", "2016-04-05"))
                             , name = c("Initial Amount", "Paycheck", "Rent", "Paycheck", "Rent", "Paycheck", "Rent", "Paycheck", "Rent")
                             , amount = c(1000, 1000, -500, 1000, -500, 1000, -500, 1000, -500)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(1000, 2000, 1500, 2500, 2000, 3000, 2500, 3500, 3000)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - initial", {
  my_budget <- update_budget(my_budget, initial=0)
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( day = c("1", "1", "5", "1", "5", "1", "5")
                             , date = as.Date(c("2016-01-01", "2016-01-01", "2016-01-05", "2016-02-01", "2016-02-05", "2016-03-01", "2016-03-05"))
                             , name = c("Initial Amount", "Paycheck", "Rent", "Paycheck", "Rent", "Paycheck", "Rent")
                             , amount = c(0, 1000, -500, 1000, -500, 1000, -500)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(0, 1000, 500, 1500, 1000, 2000, 1500)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - all", {
  my_budget <- update_budget( my_budget
                            , start=as.Date("2016-03-01")
                            , end=as.Date("2016-05-31")
                            , initial=0
                            )
  expect_identical(is.budget(my_budget), TRUE)
  expect_identical(is.list(my_budget), TRUE)
  expect_identical(my_budget$df,
                   data.frame( day = c("1", "1", "5", "1", "5", "1", "5")
                             , date = as.Date(c("2016-03-01", "2016-03-01", "2016-03-05", "2016-04-01", "2016-04-05", "2016-05-01", "2016-05-05"))
                             , name = c("Initial Amount", "Paycheck", "Rent", "Paycheck", "Rent", "Paycheck", "Rent")
                             , amount = c(0, 1000, -500, 1000, -500, 1000, -500)
                             , recurring = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
                             , balance = c(0, 1000, 500, 1500, 1000, 2000, 1500)
                             , stringsAsFactors = FALSE
                             ))
})
