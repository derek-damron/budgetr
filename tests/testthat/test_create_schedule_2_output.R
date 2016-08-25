context('create_schedule - Output')

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
my_bills_items <- create_schedule(paycheck, rent)

test_that("Check - Items", {
  expect_identical(is.schedule(my_bills_items), TRUE)
  expect_identical(is.list(my_bills_items), TRUE)
  expect_identical(my_bills_items$df,
                   data.frame( name = c("Paycheck", "Rent")
                             , amount = c(1000, -500)
                             , day = c("1", "5")
                             , recurring = c(TRUE, TRUE)
                             , stringsAsFactors=FALSE
                             ))
})

my_bills_list <- create_schedule(list(paycheck, rent))

test_that("Check - List", {
  expect_identical(is.schedule(my_bills_list), TRUE)
  expect_identical(is.list(my_bills_list), TRUE)
  expect_identical(my_bills_list$df,
                   data.frame( name = c("Paycheck", "Rent")
                             , amount = c(1000, -500)
                             , day = c("1", "5")
                             , recurring = c(TRUE, TRUE)
                             , stringsAsFactors=FALSE
                             ))
})
