context("extend_item")

paycheck <- create_item( name = "Paycheck"
                       , amount = 1000
                       , day = "2016-01-01"
                       , recurring = "monthly"
                       )

test_that("Check - day < start", {
    paycheck_extended <- extend_item( paycheck
                                    , start = as.Date("2016-02-01")
                                    , end = as.Date("2016-03-31")
                                    )
    expect_equal(paycheck_extended,
                 data.frame( date = as.Date(c("2016-02-01", "2016-03-01"))
                           , name = "Paycheck"
                           , amount = 1000
                           , stringsAsFactors = FALSE
                           ))
})

test_that("Check - start == day", {
    paycheck_extended <- extend_item( paycheck
                                    , start = as.Date("2016-01-01")
                                    , end = as.Date("2016-03-31")
                                    )
    expect_equal(paycheck_extended,
                 data.frame( date = as.Date(c("2016-01-01", "2016-02-01", "2016-03-01"))
                           , name = "Paycheck"
                           , amount = 1000
                           , stringsAsFactors = FALSE
                           ))
})

test_that("Check - start < day < end", {
    paycheck_extended <- extend_item( paycheck
                                    , start = as.Date("2016-01-01")
                                    , end = as.Date("2016-03-31")
                                    )
    expect_equal(paycheck_extended,
                 data.frame( date = as.Date(c("2016-01-01", "2016-02-01", "2016-03-01"))
                           , name = "Paycheck"
                           , amount = 1000
                           , stringsAsFactors = FALSE
                           ))
})

test_that("Check - day == end", {
    paycheck_extended <- extend_item( paycheck
                                    , start = as.Date("2015-12-01")
                                    , end = as.Date("2016-01-01")
                                    )
    expect_equal(paycheck_extended,
                 data.frame( date = as.Date("2016-01-01")
                           , name = "Paycheck"
                           , amount = 1000
                           , stringsAsFactors = FALSE
                           ))
})

test_that("Check - end < day", {
    paycheck_extended <- extend_item( paycheck
                                    , start = as.Date("2015-11-01")
                                    , end = as.Date("2015-12-01")
                                    )
    expect_equal(paycheck_extended,
                 NULL)
})
