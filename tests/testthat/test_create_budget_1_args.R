context("create_budget - Arguments")

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
  expect_error(create_budget(),
               "Please provide a schedule for your budget")
  expect_error(create_budget(1),
               "The object provided is not a schedule")
})

test_that("Check - start", {
  expect_error(create_budget(my_schedule, 1:2),
               "start must be a single value")
  expect_error(create_budget(my_schedule, 1),
               "Could not convert start into a Date object using lubridate::ymd")
  expect_equal(create_budget(my_schedule, as.Date("2016-01-01"), initial=0)$args$start
              , as.Date("2016-01-01"))
  expect_equal(create_budget(my_schedule, "2016-01-01", initial=0)$args$start
              , as.Date("2016-01-01"))
  expect_equal(create_budget(my_schedule, "20160101", initial=0)$args$start
              , as.Date("2016-01-01"))
  expect_equal(create_budget(my_schedule, 20160101, initial=0)$args$start
              , as.Date("2016-01-01"))
})

test_that("Check - end", {
  expect_error(create_budget(my_schedule, end=1:2),
               "end must be a single value")
  expect_error(create_budget(my_schedule, end=1),
               "Could not convert end into a Date object using lubridate::ymd")
  expect_error(create_budget(my_schedule, end=Sys.Date()-1),
               "end must be at least one day after start")
  expect_equal(create_budget(my_schedule, start="2016-01-01", end=as.Date("2016-01-05"), initial=0)$args$end
              , as.Date("2016-01-05"))
  expect_equal(create_budget(my_schedule, start="2016-01-01", end="2016-01-05", initial=0)$args$end
              , as.Date("2016-01-05"))
  expect_equal(create_budget(my_schedule, start="2016-01-01", end="20160105", initial=0)$args$end
              , as.Date("2016-01-05"))
  expect_equal(create_budget(my_schedule, start="2016-01-01", end=20160105, initial=0)$args$end
              , as.Date("2016-01-05"))
})

test_that("Check - initial", {
  expect_error(create_budget(my_schedule),
               "Please provide an initial amount for the budget")
  expect_error(create_budget(my_schedule, initial=1:2),
               "initial must be a single value")
  expect_error(create_budget(my_schedule, initial="a"),
               "Could not convert initial into a numeric value")
  expect_error(create_budget(my_schedule, initial=0),
               "No item in the schedule applies between start and end")
  expect_equal(create_budget(my_schedule, start="2016-01-01", end="2016-02-01", initial="1000")$args$initial,
               1000)
})
