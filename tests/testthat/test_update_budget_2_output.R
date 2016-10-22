context('update_budget - Output')

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
groceries <- create_item( name = "Groceries"
                        , amount = -100
                        , day ="2015-12-15"
                        , recurring = "2 weeks"
                        )
xmas_gifts <- create_item( name = "Christmas Gifts"
                         , amount = -500
                         , day = "2015-12-20"
                         )
my_schedule <- create_schedule(paycheck, rent, groceries, xmas_gifts)
my_budget <- create_budget( my_schedule
                          , start = "2015-12-15"
                          , end = "2016-03-02"
                          , initial = 1000
                          )

test_that("Check - nothing", {
    my_budget <- update_budget(my_budget)
    expect_identical(is.budget(my_budget), TRUE)
    expect_identical(is.list(my_budget), TRUE)

    expected_date <- as.Date(c("2015-12-15", "2015-12-15", "2015-12-20", "2015-12-29", "2016-01-01", "2016-01-05", "2016-01-12", "2016-01-26", "2016-02-01", "2016-02-05", "2016-02-09", "2016-02-23", "2016-03-01"))
    expected_name <- c("Initial Amount", "Groceries", "Christmas Gifts", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck")
    expected_amount <- c(1000, -100, -500, -100, 1000, -500, -100, -100, 1000, -500, -100, -100, 1000)
    expected_balance <- c(1000, 900, 400, 300, 1300, 800, 700, 600, 1600, 1100, 1000, 900, 1900)
    expect_identical(my_budget$df,
                     data.frame( date = expected_date
                               , name = expected_name
                               , amount = expected_amount
                               , balance = expected_balance
                               , stringsAsFactors = FALSE
                               ))
    expect_identical(my_budget$args,
                     list(start = as.Date("2015-12-15")
                         , end = as.Date("2016-03-02")
                         , initial = 1000
                         ))
})

test_that("Check - start (backwards)", {
    my_budget <- update_budget(my_budget, start="2015-11-15")
    expect_identical(is.budget(my_budget), TRUE)
    expect_identical(is.list(my_budget), TRUE)

    expected_date <- as.Date(c("2015-11-15", "2015-12-15", "2015-12-20", "2015-12-29", "2016-01-01", "2016-01-05", "2016-01-12", "2016-01-26", "2016-02-01", "2016-02-05", "2016-02-09", "2016-02-23", "2016-03-01"))
    expected_name <- c("Initial Amount", "Groceries", "Christmas Gifts", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck")
    expected_amount <- c(1000, -100, -500, -100, 1000, -500, -100, -100, 1000, -500, -100, -100, 1000)
    expected_balance <- c(1000, 900, 400, 300, 1300, 800, 700, 600, 1600, 1100, 1000, 900, 1900)
    expect_identical(my_budget$df,
                     data.frame( date = expected_date
                               , name = expected_name
                               , amount = expected_amount
                               , balance = expected_balance
                               , stringsAsFactors = FALSE
                               ))
    expect_identical(my_budget$args,
                     list(start = as.Date("2015-11-15")
                         , end = as.Date("2016-03-02")
                         , initial = 1000
                         ))
})

test_that("Check - end (backwards)", {
    my_budget <- update_budget(my_budget, end="2016-02-15")
    expect_identical(is.budget(my_budget), TRUE)
    expect_identical(is.list(my_budget), TRUE)

    expected_date <- as.Date(c("2015-12-15", "2015-12-15", "2015-12-20", "2015-12-29", "2016-01-01", "2016-01-05", "2016-01-12", "2016-01-26", "2016-02-01", "2016-02-05", "2016-02-09"))
    expected_name <- c("Initial Amount", "Groceries", "Christmas Gifts", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck", "Rent", "Groceries")
    expected_amount <- c(1000, -100, -500, -100, 1000, -500, -100, -100, 1000, -500, -100)
    expected_balance <- c(1000, 900, 400, 300, 1300, 800, 700, 600, 1600, 1100, 1000)
    expect_identical(my_budget$df,
                     data.frame( date = expected_date
                               , name = expected_name
                               , amount = expected_amount
                               , balance = expected_balance
                               , stringsAsFactors = FALSE
                               ))
    expect_identical(my_budget$args,
                     list(start = as.Date("2015-12-15")
                         , end = as.Date("2016-02-15")
                         , initial = 1000
                         ))
})

test_that("Check - initial", {
    my_budget <- update_budget(my_budget, initial=2000)
    expect_identical(is.budget(my_budget), TRUE)
    expect_identical(is.list(my_budget), TRUE)

    expected_date <- as.Date(c("2015-12-15", "2015-12-15", "2015-12-20", "2015-12-29", "2016-01-01", "2016-01-05", "2016-01-12", "2016-01-26", "2016-02-01", "2016-02-05", "2016-02-09", "2016-02-23", "2016-03-01"))
    expected_name <- c("Initial Amount", "Groceries", "Christmas Gifts", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck")
    expected_amount <- c(2000, -100, -500, -100, 1000, -500, -100, -100, 1000, -500, -100, -100, 1000)
    expected_balance <- c(1000, 900, 400, 300, 1300, 800, 700, 600, 1600, 1100, 1000, 900, 1900) + 1000
    expect_identical(my_budget$df,
                     data.frame( date = expected_date
                               , name = expected_name
                               , amount = expected_amount
                               , balance = expected_balance
                               , stringsAsFactors = FALSE
                               ))
    expect_identical(my_budget$args,
                     list(start = as.Date("2015-12-15")
                         , end = as.Date("2016-03-02")
                         , initial = 2000
                         ))
})

test_that("Check - all", {
    my_budget <- update_budget( my_budget
                            , start = "2015-11-15"
                            , end = "2016-02-15"
                            , initial = 2000
                            )
    expect_identical(is.budget(my_budget), TRUE)
    expect_identical(is.list(my_budget), TRUE)

    expected_date <- as.Date(c("2015-11-15", "2015-12-15", "2015-12-20", "2015-12-29", "2016-01-01", "2016-01-05", "2016-01-12", "2016-01-26", "2016-02-01", "2016-02-05", "2016-02-09"))
    expected_name <- c("Initial Amount", "Groceries", "Christmas Gifts", "Groceries", "Paycheck", "Rent", "Groceries", "Groceries", "Paycheck", "Rent", "Groceries")
    expected_amount <- c(2000, -100, -500, -100, 1000, -500, -100, -100, 1000, -500, -100)
    expected_balance <- c(1000, 900, 400, 300, 1300, 800, 700, 600, 1600, 1100, 1000) + 1000
    expect_identical(my_budget$df,
                     data.frame( date = expected_date
                               , name = expected_name
                               , amount = expected_amount
                               , balance = expected_balance
                               , stringsAsFactors = FALSE
                               ))
    expect_identical(my_budget$args,
                     list(start = as.Date("2015-11-15")
                         , end = as.Date("2016-02-15")
                         , initial = 2000
                         ))
})
