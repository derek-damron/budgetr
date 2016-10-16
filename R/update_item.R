#' Updates a budget item
#'
#' \code{update_item} returns an updated budget item.
#'
#' @param item The budget item to be updated.
#' @param name The new name for the budget item (if applicable).
#' @param amount The new amount for the budget item (if applicable).
#' @param day The new day for the budget item (if applicable).
#' @param recurring The new recurring status for the budget item (if applicable).
#' @return The output of \code{update_item} is a new budget item with the specified updates.
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
#'
#' # Update the paycheck amount
#' paycheck <- update_item( paycheck
#'                        , amount = 1500
#'                        )
#' # Inspect
#' paycheck

update_item <- function(item, name, amount, day, recurring) {
    # Check item
    if (missing(item)) {
        stop("Please provide a budget item to update", call.=FALSE)
    # } else if (length(item) != 1) {
    #     stop("Only one budget item can be updated at a time", call.=FALSE)
    } else if (!is.item(item)) {
        stop("item must be a budget item", call.=FALSE)
    }

    # Check name
    if (missing(name)) {
        name <- item$name
    }

    # Check amount
    if (missing(amount)) {
        amount <- item$amount
    }

    # Check day
    if (missing(day)) {
        day <- item$day
    }

    # Check recurring
    if (missing(recurring)) {
        recurring <- item$recurring
    }

    # Update item
    create_item( name = name
               , amount = amount
               , day = day
               , recurring = recurring
               )
}
