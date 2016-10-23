extend_item <- function(item, start, end) {
    # Convenience variables
    day <- item$day
    recurring <- item$recurring

    # Parse recurring
    recurring <- parse_recurring(recurring)
    recurring_time <- as.numeric(recurring[1])
    recurring_length <- recurring[2]
    recurring_period <- do.call(recurring_length, list(recurring_time))

    # Return nothing if end < day
    if (end < day) {
        return(NULL)
    }

    # Derive time between day and end
    item_int <- lubridate::interval(start=day, end=end)
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
                             , stringsAsFactors = FALSE
                             )

    # Truncate extended item if day < start
    if (day < start) {
        item_extend <- item_extend[item_extend$date >= start,]
        rownames(item_extend) <- NULL
    }

    item_extend
}
