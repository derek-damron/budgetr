#' Updates a budget
#'
#' \code{update_budget} returns an updated budget.
#'
#' @param budget The budget to be updated.
#' @param start The new start date for the budget.
#' @param end The new end date for the budget.
#' @param initial The new initial amount for the budget.
#' @return The output of \code{update_budget} is a new budget with the specified updates.
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
#'#' # Create a budget
#' my_budget <- create_budget(my_schedule, initial=1000)
#' # Inspect
#' my_budget
#'
#' # Update the initial amount
#' my_budget <- update_budget(my_budget, initial=0)
#' # Inspect
#' my_budget

update_budget <- function(budget, start, end, initial) {
    # Check budget
    if (missing(budget)) {
        stop("Please provide a budget to update", call.=FALSE)
    } else if (!is.budget(budget)) {
        stop("budget must be a budget", call.=FALSE)
    }

    # Check start
    if (missing(start)) {
        start <- budget$args$start
    }

    # Check end
    if (missing(end)) {
        end <- start + 90
    }

    # Check initial
    if (missing(initial)) {
        initial <- budget$args$initial
    }

    # Update budget
    create_budget( schedule = budget$schedule
                 , start = start
                 , end = end
                 , initial = initial
                 )
}
