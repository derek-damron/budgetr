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
#' my_budget <- create_budget(my_schedule, initial=1000)
#' # Inspect
#' my_budget

create_budget <- function(schedule, start=Sys.Date(), end=start+90, initial=0) {
    # Check schedule
    if (missing(schedule)) {
        stop("Please provide a schedule for your budget", call.=FALSE)
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

    #####
    # Create the budget!
    #

    # Set up a data.frame with all days within the budget range
    budget <- data.frame(budget_date = seq(start, end, by=1))
    budget$budget_year <- as.numeric(format(budget$budget_date, "%Y"))
    budget$budget_month <- as.numeric(format(budget$budget_date, "%m"))
    budget$budget_day <- as.numeric(format(budget$budget_date, "%d"))

    # Aggregate and calculate the first/last included day of each month
    budget <- aggregate( budget_day ~ budget_year + budget_month
                       , budget
                       , function(x) c(first = min(x), last = max(x))
                       )
    budget <- as.data.frame(as.list(budget), stringsAsFactors = FALSE)
    budget <- budget[order(budget$budget_year, budget$budget_month), ]
    names(budget) <- sub(".", "_", names(budget), fixed = TRUE)

    # Derive the actual last day of each month in the budget
    budget$actual_day_last <- with(budget, last_day_of_month(budget_year, budget_month))

    # Create a new set of rows for each month/year combination
    budget <- budget[ rep( 1:nrow(budget)
                         , each = nrow(schedule$df)
                         ), ]

    # Add schedule columns (thank you R recycling!)
    budget <- cbind(budget, schedule$df)

    # Convert day into a numeric value based on year, month, first day, and last day!
    budget$day_mapped <- with(budget, map_days( budget_year
                                              , budget_month
                                              , budget_day_first
                                              , budget_day_last
                                              , day
                                              ))

    # Subset based on budget first/last days
    budget <- subset(budget, day_mapped >= budget_day_first)
    budget <- subset(budget, day_mapped <= budget_day_last)
    # Subset based on actual last day
    budget <- subset(budget, !(day == "last" & day_mapped != actual_day_last))

    # Derive the date for each row
    budget$date <- with(budget, as.Date( paste(budget_year, budget_month, day_mapped, sep="-")
                                              , format = "%Y-%m-%d"
                                              ))

    # Subset and reorder columns
    budget <- subset(budget, select=c( date
                                     , name
                                     , amount
                                     , recurring
                                     ))
    # Initialize balance
    budget$balance <- NA_real_

    # Add initial row
    initial_row <- data.frame( date = start
                             , name = "Initial Amount"
                             , amount = initial
                             , recurring = FALSE
                             , balance = NA_real_
                             , stringsAsFactors = FALSE
                             )
    budget <- rbind(initial_row, budget)

    # Derive balance
    budget$balance <- cumsum(budget$amount)

    # Reset row names
    row.names(budget) <- NULL

    #####
    # Objectify!
    #

    budget <- list( df = budget
                  , args = list( start = start
                               , end = end
                               , initial = initial
                               )
                  , schedule = schedule
                  )
    class(budget) <- c("budget", "list")
    budget
}
