#' Prints a clean version of a budget
#'
#' \code{print.budget} prints a clean version of a budget.
#'
#' @param x A budget.
#' @export

print.budget <- function(x) {
    writeLines("budget")
    print(x$df, row.names=FALSE)
    invisible(NULL)
}
