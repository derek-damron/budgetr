#' Updates a budget schedule
#'
#' \code{updates_schedule} returns an updated budget schedule.
#'
#' @param schedule The budget schedule to be updated.
#' @param add A list of budget items to add to the schedule.
#' @param remove A list of budget items to remove from the schedule.
#' @return The output of \code{update_schedule} is a new budget schedule with the specified updates.
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
#' # Inspect
#' my_schedule
#'
#' # Add an internet item to the schedule
#' internet <- create_item( name = "Internet"
#'                        , amount = -100
#'                        , day = 15
#'                        , recurring = TRUE
#'                        )
#' my_schedule <- update_schedule(my_schedule, add=internet)
#' # Inspect
#' my_schedule
#'
#' # Remove the internet item from the schedule
#' my_schedule <- update_schedule(my_schedule, remove=internet)
#' # Inspect
#' my_schedule

update_schedule <- function(schedule, add, remove) {
    # Check schedule
    if (missing(schedule)) {
        stop("Please provide a budget schedule to update", call.=FALSE)
    } else if (!is.schedule(schedule)) {
        stop("schedule must be a budget schedule", call.=FALSE)
    }

    # Check add
    if (missing(add)) {
        add <- NULL
    } else if (is.item(add)) {
        add <- list(add)
    } else if (!is.list(add)) {
        add <- list(add)
    }
    add_check <- sapply(add, is.item)
    if (any(!add_check)) {
        stop("At least one of the add objects provided isn't a budget item", call.=FALSE)
    }

    # Check remove
    if (missing(remove)) {
        remove <- NULL
    } else if (is.item(remove)) {
        remove <- list(remove)
    } else if (!is.list(remove)) {
        remove <- list(remove)
    }
    remove_check <- sapply(remove, is.item)
    if (any(!remove_check)) {
        stop("At least one of the remove objects provided isn't a budget item", call.=FALSE)
    }

    # Extract items
    items <- schedule$items

    # Add items
    items <- c(items, add)

    # Remove items
    for (r in remove) {
        items_remove <- sapply(items, function(x) !identical(x, r))
        items <- items[items_remove]
    }

    # Update schedule
    create_schedule(items)
}
