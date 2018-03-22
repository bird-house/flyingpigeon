##' Year length associated with a model calendar
##'
##' Returns the length of year (in days) associated with a particular
##' calendar.  The input can be a string naming a type of calendar, or
##' a variable with an attribute named 'calendar' holding such a
##' string.
##'
##' Climate models often use a year with a length other than the
##' real-world value of 365.2425 days.  In netCDF files that comply
##' with the CF Metadata Standard, this is recorded as the 'calendar'
##' attribute of the time coordinate variable.  Valid calendars are:
##'
##' \tabular{ll}{
##' standard, gregorian: \tab 365.2425 days \cr
##' proleptic_gregorian: \tab 365.2425 days \cr
##' julian:              \tab 365.25 days   \cr
##' noleap, 365_day:     \tab 365 days      \cr
##' all_leap, 366_day:   \tab 366 days      \cr
##' 360_day:             \tab 360 days
##' }
##' 
##' Note that although the real-world Gregorian calendar is a Julian
##' calendar before October 15, 1582, accounting for this change would
##' be challenging and would frequently run contrary to common usage,
##' and so this function does not do so.
##'
##' @param time A variable with an attribute named 'calendar' or a
##' string.  The value of the attribute should be a string denoting
##' the type of calendar according to the CF Metdata Conventions.
##'
##' @return The length of the year for the given calendar, in days.
##' If the calendar is unknown, a warning is thrown and NA is
##' returned.  If \code{time} is a variable without a calendar, the
##' default year length of 365.2425 days is returned.
##'
##' @examples
##' yearlength("gregorian")
##' x <- seq(100)
##' yearlength(x)
##' x@@calendar <- "noleap"
##' yearlength(x)
##' x@@calendar <- "martian"
##' yearlength(x)
##'
##' @export


yearlength <- function(time){
    calendar <- time@calendar

    if(is.null(calendar)){
        if(is.character(time)){
            calendar <- time
        } else{
            return(365.2425)
        }
    }
    calendar <- tolower(calendar)
        
    if(calendar %in% c("proleptic_gregorian",
                       "standard",
                       "gregorian")) return(365.2425)

    if(calendar %in% c("julian"))    return(365.25)

    if(calendar %in% c("noleap",
                       "365_day"))   return(365)

    if(calendar %in% c("all_leap",
                       "366_day"))   return(366)

    if(calendar %in% c("360_day"))   return(360)

    warning(paste("unknown calendar:",calendar))
    return(NA)
}
