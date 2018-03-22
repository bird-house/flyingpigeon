##' Calculates the difference between PDF tails.
##'
##' Given two datasets (observations and model), this function
##' calculates a skill score for the agreement between the tails.  The
##' skill score is the area between the two PDFs above some threshold,
##' weighted linearly outwards.  This metric is based on Perkins, et
##' al. 2013 in the Int. J. Climatology (doi: 10.1002/joc.3500).
##'
##' The upper tail is scored if the threshold value is >= 0.5;
##' otherwise, the lower tail is scored.  The \code{bkde} function
##' from the \code{KernSmooth} package is used to estimate the PDFs of
##' the two datasets.
##'
##' @param obs A vector of observed data.
##'
##' @param mod A vector of model data.
##'
##' @param threshold The probability threshold where the tail begins.
##' Default value: 0.95.
##'
##' @param ... Arguments to the density estimation function.
##'
##' @return A numeric skill score; 1 indicates perfect agreement, 0 no
##' agreement.
##'
##' @examples
##'
##' library(KernSmooth)
##' set.seed(22)
##' obs <- rgamma(1000, shape=1, scale=1)
##' perfect <- rgamma(1000, shape=1, scale=1)
##' good <- rgamma(1000, shape=6/5, scale=5/6)
##' bad <- rgamma(1000, shape=3, scale=1/3)
##' 
##' x <- namelist(obs, perfect, good, bad)
##' 
##' mplot(lapply(x, bkde), type="l", col=c("black","blue","green","red"), lty=1)
##' 
##' legend("topright", names(x), col=c("black","blue","green","red"), lty=1, lwd=2)
##' 
##' tailskill(obs, obs)
##' tailskill(obs, perfect)
##' tailskill(obs, good)
##' tailskill(obs, bad)
##'
##' @importFrom KernSmooth bkde
##' @importFrom stats na.omit
##' 
##' @export


tailskill <- function(obs, mod, threshold = 0.95, ...){

    obs <- na.omit(obs)
    mod <- na.omit(mod)
    
    if(threshold <= 0 | threshold >= 1){
        stop("Threshold must be between 0 and 1.")
    }

    ## Which tail to score
    lower <- threshold < 0.5
    
    ## The easiest way to figure out what bkde's range.x would be for
    ## both datasets is just to call it on both of them.    
    range.x <- range(bkde(c(obs, mod))$x)
    
    obspdf <- bkde(obs, range.x=range.x)
    modpdf <- bkde(mod, range.x=range.x)

    nbins <- length(obspdf$x)
    
    ## Scaling per bin to get the PDF area to sum to 1.
    scale <- diff(range(obspdf$x)) / (nbins - 1)

    ## Don't use quantile() here!  Quantiles are hard to estimate.
    ## Finding the matching CDF value from the estimated PDF is more
    ## robust and matches the way we're using it.
    thind <- which.min(abs(cumsum(obspdf$y) * scale - threshold))

    tailbins <- seq(thind, ifelse(lower, 1, nbins))
    
    tobspdf <- lapply(obspdf, `[`, tailbins)
    tmodpdf <- lapply(modpdf, `[`, tailbins)
    
    dpdf <- abs(tobspdf$y - tmodpdf$y)

    ## Note: the factor of 10 is arbitrary, but keep it for now.
    weight <- seq(0, 1, along=tobspdf$x)*10
    if(lower){ weight <- rev(weight) }

    score <- sum(dpdf * weight)

    ## Sets the value from 0 (no skill) to 1 (perfect)
    score <- 1/(1+score)
    
    return(score)    
}
