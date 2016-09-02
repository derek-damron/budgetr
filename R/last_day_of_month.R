last_day_of_month <- function(year, month) {
    # Check year
    if (missing(year)) {
        stop("Please provide a year", call.=FALSE)
    } else if (!is.numeric(year)) {
        stop("year must be a numeric value", call.=FALSE)
    } else if (year <= 0) {
        stop("year must be a positive value", call.=FALSE)
    } else if (floor(year) != year) {
        stop("year must be an integer", call.=FALSE)
    }

    # Check month
    if (missing(month)) {
        stop("Please provide a month", call.=FALSE)
    } else if (!is.numeric(month)) {
        stop("month must be a numeric value", call.=FALSE)
    } else if (! month %in% 1:12) {
        stop("month must be a positive integer between 1 and 12", call.=FALSE)
    }

    # Start with the first day of the month
    first_of_month <- as.Date(paste(year, month, "01", sep="-"))

    # Derive the last day of the month
    last_of_month <- seq(first_of_month, length = 2, by = "month")[2] - 1

    # Extract the day as a numeric
    last_of_month <- as.numeric(format(last_of_month, "%d"))

    return(last_of_month)
}
# Vectorize across all arguments
last_day_of_month <- Vectorize(last_day_of_month)
