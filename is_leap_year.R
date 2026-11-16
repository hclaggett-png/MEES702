# A year is a leap year if it is divisible by 4,
# except for end-of-century years (divisible by 100)
# which must also be divisible by 400

is_leap_year <- function(year) {
    if (!is.numeric(year)) {
        stop("Must specify numeric value for year")
    }

    is_leap <- (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))

    return(is_leap)
}