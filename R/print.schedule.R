#' Prints a clean version of a budget schedule
#'
#' \code{print.schedule} prints a clean version of a budget schedule.
#'
#' @param x A budget schedule.
#' @export

print.schedule <- function(x) {
    writeLines("budget schedule")
    print(x$df, row.names=FALSE)
    invisible(NULL)
}
