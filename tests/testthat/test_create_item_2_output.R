context('create_item - Output')

name <- "Rent"
amount <- -800
day <- 1
recurring <- TRUE

rent <- create_item(name, amount, day, recurring)

test_that("Check - Output", {
  expect_identical(is.item(rent), TRUE)
  expect_identical(is.list(rent), TRUE)
  expect_identical(rent$df,
                   data.frame( name = name
                             , amount = amount
                             , day = as.character(day)
                             , recurring = recurring
                             , stringsAsFactors=FALSE
                             ))
})
