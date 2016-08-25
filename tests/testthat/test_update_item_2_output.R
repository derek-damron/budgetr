context('update_item - Output')

name <- "Rent"
amount <- -500
day <- 1
recurring <- TRUE
rent <- create_item(name, amount, day, recurring)

test_that("Check - nothing", {
  rent <- update_item(rent)
  expect_identical(rent$df,
                   data.frame( name = name
                             , amount = amount
                             , day = as.character(day)
                             , recurring = recurring
                             , stringsAsFactors = FALSE
                             ))
})

name_update <- "Rent 2"
amount_update <- -750
day_update <- 2
recurring_update <- FALSE

test_that("Check - name", {
  rent <- update_item(rent, name = name_update)
  expect_identical(rent$df,
                   data.frame( name = name_update
                             , amount = amount
                             , day = as.character(day)
                             , recurring = recurring
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - amount", {
  rent <- update_item(rent, amount = amount_update)
  expect_identical(rent$df,
                   data.frame( name = name
                             , amount = amount_update
                             , day = as.character(day)
                             , recurring = recurring
                             , stringsAsFactors = FALSE
                             ))
})


test_that("Check - day", {
  rent <- update_item(rent, day = day_update)
  expect_identical(rent$df,
                   data.frame( name = name
                             , amount = amount
                             , day = as.character(day_update)
                             , recurring = recurring
                             , stringsAsFactors = FALSE
                             ))
})


test_that("Check - recurring", {
  rent <- update_item(rent, recurring = recurring_update)
  expect_identical(rent$df,
                   data.frame( name = name
                             , amount = amount
                             , day = as.character(day)
                             , recurring = recurring_update
                             , stringsAsFactors = FALSE
                             ))
})

test_that("Check - everything", {
  rent <- update_item( rent
                     , name = name_update
                     , amount = amount_update
                     , day = day_update
                     , recurring = recurring_update
                     )
  expect_identical(rent$df,
                   data.frame( name = name_update
                             , amount = amount_update
                             , day = as.character(day_update)
                             , recurring = recurring_update
                             , stringsAsFactors = FALSE
                             ))
})
