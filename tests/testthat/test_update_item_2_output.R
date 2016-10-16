context('update_item - Output')

name <- "Paycheck"
amount <- 1000
day <- "2016-01-01"
recurring <- "monthly"
paycheck <- create_item(name, amount, day, recurring)

test_that("Check - nothing", {
    paycheck <- update_item(paycheck)
    expect_identical(paycheck$name, name)
    expect_identical(paycheck$amount, amount)
    expect_identical(paycheck$day, as.Date(day))
    expect_identical(paycheck$recurring, recurring)
})

name_update <- "Paycheck 2"
amount_update <- 1500
day_update <- "2016-01-01"
recurring_update <- "2 weeks"

test_that("Check - name", {
    paycheck <- update_item(paycheck, name = name_update)
    expect_identical(paycheck$name, name_update)
    expect_identical(paycheck$amount, amount)
    expect_identical(paycheck$day, as.Date(day))
    expect_identical(paycheck$recurring, recurring)
})

test_that("Check - amount", {
    paycheck <- update_item(paycheck, amount = amount_update)
    expect_identical(paycheck$name, name)
    expect_identical(paycheck$amount, amount_update)
    expect_identical(paycheck$day, as.Date(day))
    expect_identical(paycheck$recurring, recurring)
})

test_that("Check - day", {
    paycheck <- update_item(paycheck, day = day_update)
    expect_identical(paycheck$name, name)
    expect_identical(paycheck$amount, amount)
    expect_identical(paycheck$day, as.Date(day_update))
    expect_identical(paycheck$recurring, recurring)
})


test_that("Check - recurring", {
    paycheck <- update_item(paycheck, recurring = recurring_update)
    expect_identical(paycheck$name, name)
    expect_identical(paycheck$amount, amount)
    expect_identical(paycheck$day, as.Date(day))
    expect_identical(paycheck$recurring, recurring_update)
})

test_that("Check - everything", {
    paycheck <- update_item( paycheck
                           , name = name_update
                           , amount = amount_update
                           , day = day_update
                           , recurring = recurring_update
                           )
    expect_identical(paycheck$name, name_update)
    expect_identical(paycheck$amount, amount_update)
    expect_identical(paycheck$day, as.Date(day_update))
    expect_identical(paycheck$recurring, recurring_update)
})
