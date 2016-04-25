#' Checks if an object is a budget.
#'
#' \code{is.budget} Checks if an object is a budget.
#'
#' @param x A budget.
#' @export

is.schedule <- function(x) {
    is(x, "budget")
}
