#' Checks if an object is a budget schedule.
#'
#' \code{is.schedule} Checks if an object is a budget schedule.
#'
#' @param x A budget schedule.
#' @export

is.schedule <- function(x) {
    is(x, "schedule")
}
