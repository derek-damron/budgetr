#' Creates a budget schedule
#'
#' \code{create_item} returns a budget schedule.
#'
#' @param ... A series of budget items separated by commas.
#' @return The output of \code{create_item} is a budget item.
#' @export
#' @examples
#' #####
#' # Common use cases
#' #
#'
#' # I need to pay my bills!
#' rent <- create_item( name = "Rent"
#'                    , amount = -666.67
#'                    , day = "1"
#'                    , recurring = TRUE
#'                    )
#' groceries <- create_item( name = "Groceries"
#'                         , amount = -333.33
#'                         , day = "5"
#'                         , recurring = TRUE
#'                         )
#' my_bills <- create_schedule(rent, groceries)

create_schedule <- function(...) {
    items <- list(...)

    # Check that something was provided
    if (length(items) < 1) {
        stop("Please provide at least one budget item", call.=FALSE)
    }

    # Check that all args are items
    items_check <- sapply(items, is.item)
    if (!any(items_check)) {
        stop("At least one of the objects provided isn't a budget item", call.=FALSE)
    }

    # Create schedule
    schedule_df <- data.frame(do.call(rbind, items), stringsAsFactors=FALSE)
    schedule_df <- data.frame(lapply(schedule_df, unlist), stringsAsFactors = FALSE)
    schedule_df <- schedule_df[order(schedule_df$day), ]
    schedule <- list(df = schedule_df)

    # Objectify!
    class(schedule) <- c("schedule", "list")
    schedule
}
