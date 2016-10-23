context("helpers_recurring")

test_that("Check - is_valid_value_recurring", {
    #####
    # True
    #

    expect_true(is_valid_value_recurring("no"))

    # Days
    expect_true(is_valid_value_recurring("daily"))
    expect_true(is_valid_value_recurring("1 day"))
    expect_true(is_valid_value_recurring("1 days"))
    expect_true(is_valid_value_recurring("2 days"))
    expect_true(is_valid_value_recurring("10 days"))

    # Weeks
    expect_true(is_valid_value_recurring("weekly"))
    expect_true(is_valid_value_recurring("1 week"))
    expect_true(is_valid_value_recurring("1 weeks"))
    expect_true(is_valid_value_recurring("2 weeks"))
    expect_true(is_valid_value_recurring("10 weeks"))

    # Month
    expect_true(is_valid_value_recurring("monthly"))
    expect_true(is_valid_value_recurring("1 month"))
    expect_true(is_valid_value_recurring("1 months"))
    expect_true(is_valid_value_recurring("2 months"))
    expect_true(is_valid_value_recurring("10 months"))

    # Years
    expect_true(is_valid_value_recurring("yearly"))
    expect_true(is_valid_value_recurring("1 year"))
    expect_true(is_valid_value_recurring("1 years"))
    expect_true(is_valid_value_recurring("2 years"))
    expect_true(is_valid_value_recurring("10 years"))

    #####
    # False
    #

    # Days
    expect_false(is_valid_value_recurring("10 day"))

    # Weeks
    expect_false(is_valid_value_recurring("10 week"))

    # Months
    expect_false(is_valid_value_recurring("10 month"))

    # Years
    expect_false(is_valid_value_recurring("10 year"))

    # Other
    expect_false(is_valid_value_recurring(TRUE))
    expect_false(is_valid_value_recurring(FALSE))
    expect_false(is_valid_value_recurring(NULL))
    expect_false(is_valid_value_recurring(NULL))
})

test_that("Check - standardize_recurring", {
    expect_equal(standardize_recurring("no"), "0 days")

    # Days
    expect_equal(standardize_recurring("daily"), "1 days")
    expect_equal(standardize_recurring("1 day"), "1 days")
    expect_equal(standardize_recurring("1 days"), "1 days")
    expect_equal(standardize_recurring("2 days"), "2 days")
    expect_equal(standardize_recurring("10 days"), "10 days")

    # Weeks
    expect_equal(standardize_recurring("weekly"), "1 weeks")
    expect_equal(standardize_recurring("1 week"), "1 weeks")
    expect_equal(standardize_recurring("1 weeks"), "1 weeks")
    expect_equal(standardize_recurring("2 weeks"), "2 weeks")
    expect_equal(standardize_recurring("10 weeks"), "10 weeks")

    # Month
    expect_equal(standardize_recurring("monthly"), "1 months")
    expect_equal(standardize_recurring("1 month"), "1 months")
    expect_equal(standardize_recurring("1 months"), "1 months")
    expect_equal(standardize_recurring("2 months"), "2 months")
    expect_equal(standardize_recurring("10 months"), "10 months")

    # Years
    expect_equal(standardize_recurring("yearly"), "1 years")
    expect_equal(standardize_recurring("1 year"), "1 years")
    expect_equal(standardize_recurring("1 years"), "1 years")
    expect_equal(standardize_recurring("2 years"), "2 years")
    expect_equal(standardize_recurring("10 years"), "10 years")
})

test_that("Check - parse_recurring", {
    expect_equal(parse_recurring("no"), c("0", "days"))

    # Days
    expect_equal(parse_recurring("daily"), c("1", "days"))
    expect_equal(parse_recurring("1 day"), c("1", "days"))
    expect_equal(parse_recurring("1 days"), c("1", "days"))
    expect_equal(parse_recurring("2 days"), c("2", "days"))
    expect_equal(parse_recurring("10 days"), c("10", "days"))

    # Weeks
    expect_equal(parse_recurring("weekly"), c("1", "weeks"))
    expect_equal(parse_recurring("1 week"), c("1", "weeks"))
    expect_equal(parse_recurring("1 weeks"), c("1", "weeks"))
    expect_equal(parse_recurring("2 weeks"), c("2", "weeks"))
    expect_equal(parse_recurring("10 weeks"), c("10", "weeks"))

    # Month
    expect_equal(parse_recurring("monthly"), c("1", "months"))
    expect_equal(parse_recurring("1 month"), c("1", "months"))
    expect_equal(parse_recurring("1 months"), c("1", "months"))
    expect_equal(parse_recurring("2 months"), c("2", "months"))
    expect_equal(parse_recurring("10 months"), c("10", "months"))

    # Years
    expect_equal(parse_recurring("yearly"), c("1", "years"))
    expect_equal(parse_recurring("1 year"), c("1", "years"))
    expect_equal(parse_recurring("1 years"), c("1", "years"))
    expect_equal(parse_recurring("2 years"), c("2", "years"))
    expect_equal(parse_recurring("10 years"), c("10", "years"))
})
