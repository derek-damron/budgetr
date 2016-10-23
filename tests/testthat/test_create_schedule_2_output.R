context('create_schedule - Output')

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

test_that("Check - Items", {
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

my_schedule <- create_schedule(get_items())

test_that("Check - List", {
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
