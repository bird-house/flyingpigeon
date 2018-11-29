##' Removes excess drizzle from model data.
##' 
##' Climate models typically generate too much drizzle.  Given modeled
##' and observational precipitation, \code{dedrizzle} finds a
##' threshold value that will cause the wet/dry ratio in the model
##' data to match the wet/dry ratio in the observations, then sets all
##' model values below that threshold to zero.
##'
##' \code{dedrizzle} takes two vectors of model data, one for the
##' current period and one for the future period.  It finds the
##' threshold based on the current data, and floors data below the
##' threshold for both current and future data.  If the observational
##' data has more wet (non-zero) values than the model data, the
##' threshold will be zero.
##' 
##' Dedrizzle floors all its inputs at zero before calculation.  It
##' ignores NA values when calculating the wet/dry equalization
##' threshold.  The "fut" element of the input list is optional.
##'
##' @param x A list of vectors named "obs", "cur", and "fut".  The
##' vectors do not need to be the same length.
##'
##' @param att Logical: whether to set attributes "pwet" and
##' "threshold" on the returned list to record the wet/dry ratio and
##' threshold value, respectively.  (Defaults to FALSE.)
##'
##' @return A list containing obs, cur, and fut with drizzle removed.
##'
##' @examples
##' obs <- c(rep(0,10), rep(1,10), rep(NA,3))
##' cur <- c(seq(10)/10, NA)
##' fut <- seq(15)/10
##' namelist(obs,cur,fut)
##' dedrizzle(namelist(obs, cur, fut))
##' 
##' @export



dedrizzle <- function(x, att=FALSE){
    
    ## floor x at zero
    x <- lapply(x, pmax, 0)    

    ## discard NA values
    y <- lapply(x, function(z){z[!is.na(z)]})
    
    ## calculate observed wet/dry fraction
    pwet <- sum(y$obs > 0) / length(y$obs)
    
    ## find threshold equalizing model wet/dry with obs
    threshold <- sort(y$cur)[(1-pwet)*length(y$cur)]
    
    ## set values at or below threshold to zero
    x$cur[x$cur <= threshold] <- 0
    if(!is.null(x$fut)){
      x$fut[x$fut <= threshold] <- 0
    }

    if(att){
        x@pwet <- pwet        
        x@threshold <- threshold
    }
    
    return(x)
}
