#####
# Validate value
#

is_valid_value_recurring <- function(recurring) {
    if ( !is.null(recurring)
       & (  recurring == "no"
         || recurring == "daily"
         || recurring == "1 day"
         || grepl("^[1-9][0-9]* days$", recurring)
         || recurring == "weekly"
         || recurring == "1 week"
         || grepl("^[1-9][0-9]* weeks$", recurring)
         || recurring == "monthly"
         || recurring == "1 month"
         || grepl("^[1-9][0-9]* months$", recurring)
         || recurring == "yearly"
         || recurring == "1 year"
         || grepl("^[1-9][0-9]* years$", recurring)
         )
       ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#####
# Standardize value
#

standardize_recurring <- function(recurring) {
    if (recurring == "no") {
        recurring <- "0 days"
    } else if (recurring %in% c("daily", "1 day")) {
        recurring <- "1 days"
    } else if (recurring %in% c("weekly", "1 week")) {
        recurring <- "1 weeks"
    } else if (recurring %in% c("monthly", "1 month")) {
        recurring <- "1 months"
    } else if (recurring %in% c("yearly", "1 year")) {
        recurring <- "1 years"
    }

    recurring
}


#####
# Parse value
#

parse_recurring <- function(recurring) {
    # Standarize strings
    recurring_standardized <- standardize_recurring(recurring)

    # Parse
    recurring_parsed <- strsplit(recurring_standardized, split=" ")
    recurring_parsed <- do.call(c, recurring_parsed)
    recurring_parsed
}
