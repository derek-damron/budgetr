context("print_item")

rent <- create_item( name = "Rent"
                   , amount = -800
                   , day = 1
                   , recurring = TRUE
                   )

test_that("Check", {
  expect_output(print(rent),
               "budget item")
  expect_output(print(rent),
               "name amount day recurring")
  expect_output(print(rent),
               "Rent[ ]+-800[ ]+1[ ]+TRUE")
})
