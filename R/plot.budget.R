#' Plots a budget
#'
#' \code{plot.budget} plots a budget object.
#'
#' @param x A budget.
#' @return The output of \code{plot.budget} is a plot of a budget.
#' @export
#' @examples
#' #####
#' # Common use cases
#' #
#'
#' # I need to pay my bills!
#' paycheck <- create_item( name = "Paycheck"
#'                        , amount = 1050
#'                        , day = 1
#'                        , recurring = TRUE
#'                        )
#' rent <- create_item( name = "Rent"
#'                    , amount = -800
#'                    , day = 1
#'                    , recurring = TRUE
#'                    )
#' internet <- create_item( name = "Internet"
#'                        , amount = -100
#'                        , day = 15
#'                        , recurring = TRUE
#'                        )
#' my_bills <- create_schedule(paycheck, rent, internet)
#' my_budget <- create_budget(my_bills, initial=500)
#' plot(my_budget)

plot.budget <- function(x) {
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
        )
}
