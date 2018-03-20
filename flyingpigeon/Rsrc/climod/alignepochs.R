##' Adjusts the values in a CF-compliant netCDF time coordinate
##' variable to match the given representation (baseline and units).
##'
##' The CF metadata standard requires a netCDF time coordinate
##' variable to be represented as time elapsed from an epoch, e.g.,
##' "days since 1972-01-01".  Different data sources may use different
##' epochs and units that need to be aligned with one another to
##' simplify analysis; this function will align them.
##'
##' The \code{time} input variable must have an attribute named
##' "units" that follows the CF standard.  Currently supported time units for
##' conversion are "hours" and "days".
##'
##' If the input variable also has a \code{calendar} attribute, that
##' will be respected in the adjustment; otherwise, it will assume the
##' data follows a standard (365.25-day) calendar.  Recognized
##' calendars are the same as those handled by the
##' \code{\link{yearlength}} function.
##'
##' @param time A CF-style time variable
##'
##' @param epoch A CF time@@units string 
##'
##' @return A time variable with adjusted epoch and
##'
##' @examples
##' time <- seq(365)-1
##' time@units <- "days since 1950-01-01"
##' time@calendar <- "noleap"
##' str(alignepochs(time, "hours since 1949-12-01"))
##' 
##' @export

alignepochs <- function(time, epoch="days since 1950-01-01"){

  calendar <- time@calendar
  if(is.null(calendar)){calendar <- "gregorian"}

  ## ignoring HH:MM:SS for now
  pattern <- "(hours|days) since (\\d\\d\\d\\d-\\d\\d?-\\d\\d?).*"
  ustring <- strsplit(sub(pattern, "\\1 \\2", c(old=time@units, new=epoch), perl=TRUE), " ")
  units <- lapply(ustring, `[`, 1)
  date <- lapply(ustring, `[`, 2)

  if(units$old == "hours"){ time <- time / 24 }

  time <- time + diffdate(date$old, date$new, calendar)

  if(units$new == "hours"){ time <- time * 24 }

  time@units = epoch

  return(time)
}



## Currently only accepts dates in the form YYYY-MM-DD

diffdate <- function(date1, date2, calendar="gregorian"){

  if(calendar %in% c("gregorian", "standard", "proleptic_gregorian")){
    return(as.double(difftime(date1, date2, units="days")))
  }
  
  if(calendar %in% c("noleap", "365_day", "all_leap", "366_day", "360_day")){
    ymd <- rapply(strsplit(c(d1=date1, d2=date2), "-"), as.numeric, how="replace")
 
    return(yearlength(calendar)*(ymd$d1[1] - ymd$d2[1]) +
      jday(ymd$d1[2], ymd$d1[3], calendar) - 
      jday(ymd$d2[2], ymd$d2[3], calendar))
  }
  
  error(paste("Sorry, diffdate doesn't know how to handle", calendar, "calendar."))
}


jday <- function(month, day, calendar){
  std  <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
  leap <- cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30))
  unif <- cumsum(c(0,rep(30,11)))

  mlen <- list("noleap"   = std,
               "365_day"  = std,
               "all_leap" = leap,
               "366_day"  = leap,
               "360_day"  = unif)

  return(mlen[[calendar]][month]+day)
}
