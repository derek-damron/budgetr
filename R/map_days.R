map_days <- function(year, month, day_first, day_last, schedule_day) {
    # Numerics
    schedule_day <- tryCatch( as.numeric(schedule_day)
                            , error = function(e) schedule_day
                            , warning = function(w) schedule_day
                            )
    if (is.numeric(schedule_day)) {
        return(schedule_day)
    }

    # Dates

    # Characters
    if (schedule_day == "last") {
        return(day_last)
    } else {
        error_msg <- paste("Don't currently have a way to convert", schedule_day, "to a numeric day")
        stop(error_msg, .call = FALSE)
    }
}

# Vectorize across all arguments for easier calling
map_days <- Vectorize(map_days)
