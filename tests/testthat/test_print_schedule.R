context("print_schedule")

# I need to pay my bills!
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

test_that("Check", {
  expect_output(my_bills,
               "budget schedule")
  expect_output(my_bills,
               "name amount day recurring")
  expect_output(my_bills,
               "Rent[ ]+-800[ ]+1[ ]+TRUE")
  expect_output(my_bills,
               "Internet[ ]+-100[ ]+15[ ]+TRUE")
})
