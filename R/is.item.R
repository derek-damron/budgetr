#' Checks if an object is a budget item.
#'
#' \code{is.item} Checks if an object is a budget item.
#'
#' @param x An object.
#' @export

is.item <- function(x) {
    is(x, "item")
}
