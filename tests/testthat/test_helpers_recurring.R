context("helpers_recurring")

test_that("Check - is_valid_value_recurring", {
    #####
    # True
    #

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
    expect_false(is_valid_value_recurring(NULL))
    expect_false(is_valid_value_recurring(NULL))
})
