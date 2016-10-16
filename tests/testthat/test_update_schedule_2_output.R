context('update_schedule - Output')

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

test_that("Check - nothing", {
    my_schedule <- update_schedule(my_schedule)
    expect_identical(is.schedule(my_schedule), TRUE)
    expect_identical(is.list(my_schedule), TRUE)
    expect_identical(length(my_schedule), 2L)
    expect_identical(my_schedule[[1]]$name, "Paycheck")
    expect_identical(my_schedule[[1]]$amount, 1000)
    expect_identical(my_schedule[[1]]$day, as.Date("2016-01-01"))
    expect_identical(my_schedule[[1]]$recurring, "monthly")
    expect_identical(my_schedule[[2]]$name, "Rent")
    expect_identical(my_schedule[[2]]$amount, -500)
    expect_identical(my_schedule[[2]]$day, as.Date("2016-01-05"))
    expect_identical(my_schedule[[2]]$recurring, "monthly")
})

test_that("Check - add", {
    internet <- create_item( name = "Internet"
                           , amount = -100
                           , day = "2016-01-15"
                           , recurring = "monthly"
                           )
    my_schedule <- update_schedule(my_schedule, add=internet)
    expect_identical(is.schedule(my_schedule), TRUE)
    expect_identical(is.list(my_schedule), TRUE)
    expect_identical(length(my_schedule), 3L)
    expect_identical(my_schedule[[1]]$name, "Paycheck")
    expect_identical(my_schedule[[1]]$amount, 1000)
    expect_identical(my_schedule[[1]]$day, as.Date("2016-01-01"))
    expect_identical(my_schedule[[1]]$recurring, "monthly")
    expect_identical(my_schedule[[2]]$name, "Rent")
    expect_identical(my_schedule[[2]]$amount, -500)
    expect_identical(my_schedule[[2]]$day, as.Date("2016-01-05"))
    expect_identical(my_schedule[[2]]$recurring, "monthly")
    expect_identical(my_schedule[[3]]$name, "Internet")
    expect_identical(my_schedule[[3]]$amount, -100)
    expect_identical(my_schedule[[3]]$day, as.Date("2016-01-15"))
    expect_identical(my_schedule[[3]]$recurring, "monthly")
})

test_that("Check - remove", {
    my_schedule <- update_schedule(my_schedule, remove=rent)
    expect_identical(is.schedule(my_schedule), TRUE)
    expect_identical(is.list(my_schedule), TRUE)
    expect_identical(length(my_schedule), 1L)
    expect_identical(my_schedule[[1]]$name, "Paycheck")
    expect_identical(my_schedule[[1]]$amount, 1000)
    expect_identical(my_schedule[[1]]$day, as.Date("2016-01-01"))
    expect_identical(my_schedule[[1]]$recurring, "monthly")
})

test_that("Check - both", {
    internet <- create_item( name = "Internet"
                           , amount = -100
                           , day = "2016-01-15"
                           , recurring = "monthly"
                           )
    my_schedule <- update_schedule(my_schedule, add=internet, remove=paycheck)
    expect_identical(is.schedule(my_schedule), TRUE)
    expect_identical(is.list(my_schedule), TRUE)
    expect_identical(length(my_schedule), 2L)
    expect_identical(my_schedule[[1]]$name, "Rent")
    expect_identical(my_schedule[[1]]$amount, -500)
    expect_identical(my_schedule[[1]]$day, as.Date("2016-01-05"))
    expect_identical(my_schedule[[1]]$recurring, "monthly")
    expect_identical(my_schedule[[2]]$name, "Internet")
    expect_identical(my_schedule[[2]]$amount, -100)
    expect_identical(my_schedule[[2]]$day, as.Date("2016-01-15"))
    expect_identical(my_schedule[[2]]$recurring, "monthly")
})
