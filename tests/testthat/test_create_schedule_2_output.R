context('create_schedule - Output')

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
my_bills <- create_schedule(rent, internet)

test_that("Check - Output", {
  expect_identical(is.schedule(my_bills), TRUE)
  expect_identical(is.list(my_bills), TRUE)
  expect_identical(my_bills$df,
                   data.frame( name = c("Rent", "Internet")
                             , amount = c(-800, -100)
                             , day = c("1", "15")
                             , recurring = c(TRUE, TRUE)
                             , stringsAsFactors=FALSE
                             ))
})
