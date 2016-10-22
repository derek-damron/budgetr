#' Plots a budget
#'
#' \code{plot.budget} plots a budget object.
#'
#' @param x A budget.
#' @param ... Other arguments to pass to plot().
#' @return The output of \code{plot.budget} is a plot of a budget.
#' @export
#' @examples
#' # Create a paycheck item
#' paycheck <- create_item( name = "Paycheck"
#'                        , amount = 1000
#'                        , day = "2016-01-01"
#'                        , recurring = "monthly"
#'                        )
#' # Create a rent item
#' rent <- create_item( name = "Rent"
#'                    , amount = -500
#'                    , day = "2016-01-05"
#'                    , recurring = "monthly"
#'                    )
#'
#' # Create a schedule
#' my_schedule <- create_schedule(paycheck, rent)
#'
#' # Create a budget
#' my_budget <- create_budget(my_schedule, start="2016-01-01", initial=1000)
#' # Plot
#' plot(my_budget)

plot.budget <- function(x, ...) {
    # Check schedule
    if (missing(x)) {
        stop("Please provide a budget to plot", call.=FALSE)
    } else if (!is.budget(x)) {
        stop("The object provided is not a budget", call.=FALSE)
    }

    # Rollup to the day level
    x_rollup <- aggregate(amount ~ date, x$df, sum)

    # Rederive balance
    x_rollup$balance <- cumsum(x_rollup$amount)

    #####
    # Plot!
    #

    # Step plot
    plot( x = x_rollup$date
        , y = x_rollup$balance
        , type = "s"
        , xlab = "Date"
        , ylab = "Balance"
        , ylim = c( min(0, 1.1 * min(x_rollup$balance))
                  , max(0, 1.1 * max(x_rollup$balance))
                  )
        , ...
        )

    # Grey vertical line to designate zero
    abline( h = 0
          , col = "lightgrey"
          )

    # Grey vertical lines to designate months
    abline( v = unique(x_rollup$date[format(x_rollup$date, format = "%d") == "01"])
          , col = "lightgrey"
          )

    # Step plot
    lines( x = x_rollup$date
         , y = x_rollup$balance
         , type = "s"
         )

    # >= 0 points
    with( subset(x_rollup, balance >= 0)
        , points( x = date
                , y = balance
                , pch = 16
                , col = "green"
                )
        )

    # < 0 points
    with( subset(x_rollup, balance < 0)
        , points( x = date
                , y = balance
                , pch = 16
                , col = "red"
                )
        )
}
