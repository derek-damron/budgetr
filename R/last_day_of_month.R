last_day_of_month <- function(year, month) {
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
