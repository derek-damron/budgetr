#' Prints a clean version of a budget schedule
#'
#' \code{print.schedule} prints a clean version of a budget schedule.
#'
#' @param x A budget schedule.
#' @param ... Does nothing.  Included only to use the print generic.
#' @export

print.schedule <- function(x, ...) {
    writeLines("schedule (budgetr)")
    print(x$df, row.names=FALSE)
    invisible(NULL)
}
