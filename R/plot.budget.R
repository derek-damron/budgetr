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
#'                        , day = 1
#'                        , recurring = TRUE
#'                        )
#' # Create a rent item
#' rent <- create_item( name = "Rent"
#'                    , amount = -500
#'                    , day = 5
#'                    , recurring = TRUE
#'                    )
#'
#' # Create a schedule
#' my_schedule <- create_schedule(paycheck, rent)
#'
#' # Create a budget
#' my_budget <- create_budget(my_bills, initial=1000)
#' # Plot
#' plot(my_budget)

plot.budget <- function(x, ...) {
    # Check schedule
    if (missing(x)) {
        stop("Please provide a budget to plot", call.=FALSE)
    } else if (length(x) != 1) {
        stop("Only one budget should be provided", call.=FALSE)
    } else if (!is.budget(x)) {
        stop("The object provided is not a budget", call.=FALSE)
    }

    # Rollup to the day level
    x_rollup <- aggregate(balance ~ date, x$df, min)

    # Plot!
    plot( x = x_rollup$date
        , y = x_rollup$balance
        , type = "b"
        , pch = 16
        , xlab = "Date"
        , ylab = "Balance"
        , ylim = c(0, 1.1 * max(x_rollup$balance))
        , ...
        )
}
