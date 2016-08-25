context('update_schedule - Output')

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

test_that("Check - nothing", {
  my_schedule <- update_schedule(my_schedule)
  expect_identical(is.schedule(my_schedule), TRUE)
  expect_identical(is.list(my_schedule), TRUE)
  expect_identical(my_schedule$df,
                   data.frame( name = c("Paycheck", "Rent")
                             , amount = c(1000, -500)
                             , day = c("1", "5")
                             , recurring = c(TRUE, TRUE)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - add", {
  internet <- create_item( name = "Internet"
                         , amount = -100
                         , day = 15
                         , recurring = TRUE
                         )
  my_schedule <- update_schedule(my_schedule, add=internet)
  expect_identical(is.schedule(my_schedule), TRUE)
  expect_identical(is.list(my_schedule), TRUE)
  expect_identical(my_schedule$df,
                   data.frame( name = c("Paycheck", "Rent", "Internet")
                             , amount = c(1000, -500, -100)
                             , day = c("1", "5", "15")
                             , recurring = c(TRUE, TRUE, TRUE)
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - remove", {
  my_schedule <- update_schedule(my_schedule, remove=rent)
  expect_identical(is.schedule(my_schedule), TRUE)
  expect_identical(is.list(my_schedule), TRUE)
  expect_identical(my_schedule$df,
                   data.frame( name = "Paycheck"
                             , amount = 1000
                             , day = "1"
                             , recurring = TRUE
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - both", {
  internet <- create_item( name = "Internet"
                         , amount = -100
                         , day = 15
                         , recurring = TRUE
                         )
  my_schedule <- update_schedule(my_schedule, add=internet, remove=paycheck)
  expect_identical(is.schedule(my_schedule), TRUE)
  expect_identical(is.list(my_schedule), TRUE)
  expect_identical(my_schedule$df,
                   data.frame( name = c("Rent", "Internet")
                             , amount = c(-500, -100)
                             , day = c("5", "15")
                             , recurring = c(TRUE, TRUE)
                             , stringsAsFactors = FALSE
                             ))
})
