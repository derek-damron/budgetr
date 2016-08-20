#' Checks if an object is a budget.
#'
#' \code{is.budget} Checks if an object is a budget.
#'
#' @param x An object.
#' @export

is.budget <- function(x) {
    is(x, "budget")
}
