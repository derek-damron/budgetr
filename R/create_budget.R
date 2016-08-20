#' Creates a budget
#'
#' \code{create_budget} returns a budget.
#'
#' @param schedule A budget schedule.
#' @param start The start date for the budget.  The default is today's date.
#' @param end The end date for the budget.  The default is 90 days after today's date.
#' @param initial The initial amount for the budget.  The default is 0.
#' @return The output of \code{create_budget} is a budget.
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
#' # Inspect
#' my_budget

create_budget <- function(schedule, start=Sys.Date(), end=start+90, initial=0) {
    # Check schedule
    if (missing(schedule)) {
        stop("Please provide a schedule for your budget", call.=FALSE)
    } else if (length(schedule) != 1) {
        stop("Only one schedule should be provided", call.=FALSE)
    } else if (!is.schedule(schedule)) {
        stop("The object provided is not a schedule", call.=FALSE)
    }

    # Check start
    if (length(start) != 1) {
        stop("start must be a single value", call.=FALSE)
    } else if (!is(start, "Date")) {
        # Try to convert to date
        stop("start must be a Date")
    }

    # Check end
    if (length(end) != 1) {
        stop("end must be a single value", call.=FALSE)
    } else if (!is(end, "Date")) {
        # Try to convert to date
        stop("end must be a Date")
    }
    if (start >= end) {
        stop("end must be at least one day after start", call.=FALSE)
    }

    # Check initial
    if (length(initial) != 1) {
        stop("initial must be a single value", call.=FALSE)
    } else if (!is.numeric(initial)) {
        stop("initial must be a numeric value", call.=FALSE)
    }

    # Create initial row
    budget_initial <- data.frame( day = as.character(as.numeric(format(start, "%d")))
                                , date = start
                                , name = "Initial Amount"
                                , amount = initial
                                , recurring = FALSE
                                , stringsAsFactors = FALSE
                                )
    # Create budget template
    budget <- data.frame(date = seq(start, end, by=1))
    # Extract the day and remove leading zeros
    budget$day <- format(budget$date, "%d")
    budget$day <- as.character(as.numeric(budget$day))
    # Merge in schedule
    budget <- merge(budget, schedule$df, by="day")
    # Sort chronologically
    budget <- budget[order(budget$date), ]
    # Add initial row
    budget <- rbind(budget_initial, budget)
    # Calculate balance
    budget$balance <- cumsum(budget$amount)

    # Objectify!
    budget <- list(df = budget)
    class(budget) <- c("budget", "list")
    budget
}
