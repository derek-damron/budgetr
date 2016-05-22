context("print_item")

rent <- create_item( name = "Rent"
                   , amount = -800
                   , day = 1
                   , recurring = TRUE
                   )

test_that("Check", {
  expect_output(rent,
               "budget item")
  expect_output(rent,
               "name amount day recurring")
  expect_output(rent,
               "Rent[ ]+-800[ ]+1[ ]+TRUE")
})
