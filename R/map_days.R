map_days <- function(year, month, day_first, day_last, schedule_day) {
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

    # Check day_first
    if (missing(day_first)) {
        stop("Please provide a day_first", call.=FALSE)
    } else if (!is.numeric(day_first)) {
        stop("day_first must be a numeric value", call.=FALSE)
    } else if (! day_first %in% 1:31) {
        stop("day_first must be a positive integer between 1 and 31", call.=FALSE)
    }

    # Check day_last
    if (missing(day_last)) {
        stop("Please provide a day_last", call.=FALSE)
    } else if (!is.numeric(day_last)) {
        stop("day_last must be a numeric value", call.=FALSE)
    } else if (! day_last %in% 1:31) {
        stop("day_last must be a positive integer between 1 and 31", call.=FALSE)
    } else if (day_last < day_first) {
        stop("day_last must greater than or equal to day_first", call.=FALSE)
    }

    # Check schedule_day
    if (missing(schedule_day)) {
        stop("Please provide a schedule_day", call.=FALSE)
    }

    # Dates
    schedule_day <- tryCatch( as.Date(schedule_day)
                            , error = function(e) schedule_day
                            , warning = function(w) schedule_day
                            )
    if (is(schedule_day, "Date")) {
        # Extract year and month
        schedule_year <- as.numeric(format(schedule_day, format="%Y"))
        schedule_month <- as.numeric(format(schedule_day, format="%m"))

        # Compare
        if (year == schedule_year & month == schedule_month) {
            return(as.numeric(format(schedule_day, format="%d")))
        } else {
            return(NA_real_)
        }
    }

    # Numerics
    schedule_day <- tryCatch( as.numeric(schedule_day)
                            , error = function(e) schedule_day
                            , warning = function(w) schedule_day
                            )
    if (is.numeric(schedule_day)) {
        return(schedule_day)
    }

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
