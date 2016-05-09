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
#'                    , amount = -800
#'                    , day = 1
#'                    , recurring = TRUE
#'                    )
#' internet <- create_item( name = "Internet"
#'                        , amount = -100
#'                        , day = 15
#'                        , recurring = TRUE
#'                        )
#' my_bills <- create_schedule(rent, internet)
#' my_bills

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
    items_dfs <- lapply(items, function(x) {x$df})
    schedule_df <- do.call(rbind, items_dfs)
    schedule_df <- schedule_df[order(schedule_df$day), ]
    schedule <- list(df = schedule_df)

    # Objectify!
    class(schedule) <- c("schedule", "list")
    schedule
}
