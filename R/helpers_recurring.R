#####
# Validate value
#

is_valid_value_recurring <- function(recurring) {
    if ( !is.null(recurring)
       & (  recurring == "daily"
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

