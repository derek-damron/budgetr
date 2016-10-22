extend_item <- function(item, start, end) {
    # Convenience variables
    day <- item$day
    recurring <- item$recurring

    # Parse recurring
    recurring <- parse_recurring(recurring)
    recurring_time <- as.numeric(recurring[1])
    recurring_length <- recurring[2]
    recurring_period <- do.call(recurring_length, list(recurring_time))

    if (day < start) {
        return(NULL)
    } else if (start <= day & day <= end) {
        # Derive time between day and end
        item_int <- interval(start=day, end=end)
        if (as.numeric(recurring_period) == 0) {
            item_int_time <- 0
        } else {
            item_int_time <- item_int / recurring_period
        }
        item_int_time_floor <- floor(item_int_time)

        # Extend item
        item_extend_dates <- day + do.call(recurring_length, list(0:item_int_time_floor * recurring_time))
        item_extend <- data.frame( date = item_extend_dates
                                 , name = item$name
                                 , amount = item$amount
                                 )
    } else {
        return(NULL)
    }

    item_extend
}
