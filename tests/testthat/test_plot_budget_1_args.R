context("plot_budget - Arguments")

test_that("Check", {
  expect_error(plot.budget(),
              "Please provide a budget to plot")
  expect_error(plot.budget(1),
              "The object provided is not a budget")
})
