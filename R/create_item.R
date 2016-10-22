#' Creates a budget item
#'
#' \code{create_item} returns a budget item.
#'
#' \strong{day}
#'
#' The day argument must be one of the following types:
#'
#' \itemize{
#'   \item A Date object
#'   \item A character or numeric object that can be converted by lubridate's
#'         \code{\link[lubridate]{ymd}} function.
#' }
#'
#' \strong{recurring}
#'
#' No recurring argument means that the item is one-time (the default). If provided, recurring
#' must be one of the following values:
#'
#' \itemize{
#'   \item "daily" or "1 day" (recur every day)
#'   \item "X days" (recur every X days)
#'   \item "weekly" or "1 week" (recur every week)
#'   \item "X weeks" (recur every X weeks)
#'   \item "monthly" or "1 month" (recur every month)
#'   \item "X months" (recur every X months)
#'   \item "yearly" or "1 year" (recur every year)
#'   \item "X years" (recur every X years)
#' }
#'
#' @param name The name describing the budget item.
#' @param amount The amount associated with the budget item.  Positive values result in addition
#'   and negative values result in subtraction.
#' @param day The day associated with the budget item.  See Details for more information.
#' @param recurring The frequency of recurrence for the budget item.  See Details for more information.
#' @return The output of \code{create_item} is a budget item.
#' @export
#' @examples
#' # Create a paycheck item
#' paycheck <- create_item( name = "Paycheck"
#'                        , amount = 1000
#'                        , day = "2016-01-01"
#'                        , recurring = "monthly"
#'                        )
#' # Inspect
#' paycheck

create_item <- function(name, amount, day, recurring) {
    # Check name
    if (missing(name)) {
        stop("Please provide a name for the budget item", call.=FALSE)
    } else if (length(name) != 1) {
        stop("name must be a single value", call.=FALSE)
    } else if (!is.character(name)) {
        # Try to convert to a character
        name <- tryCatch( as.character(name)
                        , warning = function(w) name
                        , error = function(e) name
                        )
        if (!is.character(name)) {
            stop("Could not convert name into a character value", call.=FALSE)
        }
    }

    # Check amount
    if (missing(amount)) {
        stop("Please provide an amount for the budget item", call.=FALSE)
    } else if (length(amount) != 1) {
        stop("amount must be a single value", call.=FALSE)
    } else if (!is.numeric(amount)) {
        # Try to convert to a numeric
        amount <- tryCatch( as.numeric(amount)
                          , warning = function(w) amount
                          , error = function(e) amount
                          )
        if (!is.numeric(amount)) {
            stop("Could not convert amount into a numeric value", call.=FALSE)
        }
    }

    # Check day
    if (missing(day)) {
        stop("Please provide a day for the budget item", call.=FALSE)
    } else if (length(day) != 1) {
        stop("day must be a single value", call.=FALSE)
    } else if (!is.Date(day)) {
        # Try to convert to a Date using lubridate::ymd
        day <- tryCatch( lubridate::ymd(day)
                       , warning = function(w) day
                       , error = function(e) day
                       )
        if (!is.Date(day)) {
            stop("Could not convert day into a Date object using lubridate::ymd", call.=FALSE)
        }
    }

    # Check recurring
    if (missing(recurring)) {
        recurring <- "no"
    } else if (length(recurring) != 1) {
        stop("recurring must be a single value", call.=FALSE)
    } else if (!is_valid_value_recurring(recurring)) {
        stop("recurring is not a recognized value, see Details in ?create_item for possible values", call.=FALSE)
    }

    # Create item
    item <- list( name = name
                , amount = amount
                , day = day
                , recurring = recurring
                )

    # Objectify!
    class(item) <- c("item", "list")
    item
}
