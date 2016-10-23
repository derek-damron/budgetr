context("update_schedule - Arguments")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = "2016-01-01"
                       , recurring = "monthly"
                       )
rent <- create_item( name = "Rent"
                   , amount = -500
                   , day = "2016-01-05"
                   , recurring = "monthly"
                   )
my_schedule <- create_schedule(paycheck, rent)

test_that("Check - schedule", {
  expect_error(update_schedule(),
               "Please provide a budget schedule to update")
  expect_error(update_schedule(1),
               "schedule must be a budget schedule")
})

test_that("Check - add", {
  expect_error(update_schedule(my_schedule, add=1),
               "At least one of the add objects provided isn't a budget item")
  expect_error(update_schedule(my_schedule, add=factor(letters[1:5])),
               "At least one of the add objects provided isn't a budget item")
})

test_that("Check - remove", {
  expect_error(update_schedule(my_schedule, remove=1),
               "At least one of the remove objects provided isn't a budget item")
  expect_error(update_schedule(my_schedule, remove=factor(letters[1:5])),
               "At least one of the remove objects provided isn't a budget item")
})
