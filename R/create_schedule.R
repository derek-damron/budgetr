#' Creates a budget schedule
#'
#' \code{create_schedule} returns a budget schedule.
#'
#' @param ... A series of budget items separated by commas or a single list of budget items.
#' @return The output of \code{create_schedule} is a budget item.
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
#' # Using a list of items
#' my_schedule <- create_schedule(list(paycheck, rent))
#' # Inspect
#' my_schedule

create_schedule <- function(...) {
    # Check that something was provided
    if (missing(...)) {
        stop("Please provide at least one budget item", call.=FALSE)
    }

    # Convert ... to a list of items
    items <- list(...)

    # Remove the double list structure if ... was a list rather than separate items
    if (!is.item(items[[1]])) {
        items <- items[[1]]
    }

    # Check that all args are items
    items_check <- sapply(items, is.item)
    if (any(!items_check)) {
        stop("At least one of the objects provided isn't a budget item", call.=FALSE)
    }

    # Create schedule
    items_dfs <- lapply(items, function(x) {x$df})
    schedule_df <- do.call(rbind, items_dfs)

    # Add an id column for proper tracing
    df_cols <- names(schedule_df)
    schedule_df$id <- 1:nrow(schedule_df)
    # Put id as the first column
    schedule_df <- schedule_df[c("id", df_cols)]

    # Objectify!
    schedule <- list( df = schedule_df
                    , items = items
                    )
    class(schedule) <- c("schedule", "list")
    schedule
}
