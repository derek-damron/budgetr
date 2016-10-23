#' Prints a clean version of a budget schedule
#'
#' \code{print.schedule} prints a clean version of a budget schedule.
#'
#' @param x A budget schedule.
#' @param ... Does nothing.  Included only to use the print generic.
#' @export

print.schedule <- function(x, ...) {
    # Prettify items into a data.frame
    x_df <- lapply(x, as.data.frame)
    x_df <- do.call(rbind, x_df)

    # Print
    writeLines("schedule (budgetr)")
    print(x_df, row.names=FALSE)
    invisible(NULL)
}
