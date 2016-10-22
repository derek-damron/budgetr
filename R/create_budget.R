#' Creates a budget
#'
#' \code{create_budget} returns a budget.
#'
#' @param schedule A budget schedule.
#' @param start The start date for the budget.  The default is today's date.  If provided must be
#'              either a Date object or a character or numeric object that can be converted by
#'              lubridate's \code{\link[lubridate]{ymd}} function.
#' @param end The end date for the budget.  The default is 90 days after today's date.  If provided
#'            must be either a Date object or a character or numeric object that can be converted by
#'            lubridate's \code{\link[lubridate]{ymd}} function and be after the start date.
#' @param initial The initial amount for the budget.  The default is 0.
#' @return The output of \code{create_budget} is a budget.
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
    } else if (!is.Date(start)) {
        # Try to convert to a Date using lubridate::ymd
        start <- tryCatch( lubridate::ymd(start)
                         , warning = function(w) start
                         , error = function(e) start
                         )
        if (!is.Date(start)) {
            stop("Could not convert start into a Date object using lubridate::ymd", call.=FALSE)
        }
    }

    # Check end
    if (length(end) != 1) {
        stop("end must be a single value", call.=FALSE)
    } else if (!is.Date(end)) {
        # Try to convert to a Date using lubridate::ymd
        end <- tryCatch( lubridate::ymd(end)
                       , warning = function(w) end
                       , error = function(e) end
                       )
        if (!is.Date(end)) {
            stop("Could not convert end into a Date object using lubridate::ymd", call.=FALSE)
        }
    }
    if (start >= end) {
        stop("end must be at least one day after start", call.=FALSE)
    }

    # Check initial
    if (missing(initial)) {
        stop("Please provide an initial amount for the budget", call.=FALSE)
    } else if (length(initial) != 1) {
        stop("initial must be a single value", call.=FALSE)
    } else if (!is.numeric(initial)) {
        # Try to convert to a numeric
        initial <- tryCatch( as.numeric(initial)
                           , warning = function(w) initial
                           , error = function(e) initial
                           )
        if (!is.numeric(initial)) {
            stop("Could not convert initial into a numeric value", call.=FALSE)
        }
    }

    #####
    # Create the budget!
    #

    # Extend schedule
    schedule_extended <- lapply(schedule, extend_item, start=start, end=end)

    # Collapse
    budget <- do.call(rbind, schedule_extended)

    # Exit if budget is NULL (i.e. no item gets applied)
    if (is.null(budget)) {
        stop("No item in the schedule applies between start and end", call.=FALSE)
    }

    # Sort on date
    budget <- budget[order(budget$date, decreasing = FALSE), ]

    # Initialize balance
    budget$balance <- NA_real_

    # Add initial row
    initial_row <- data.frame( date = start
                             , name = "Initial Amount"
                             , amount = initial
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
