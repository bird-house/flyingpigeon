##' Integrate a PDF to get a CDF
##'
##' Numerically integrates a PDF to get a CDF using the trapezoid rule.
##' This function is designed for use on the output of kernel density
##' estimators like \code{base::density} or \code{KernSmooth::bkde}.
##'
##' @param p The PDF to integrate, either as a vector of probabilities
##' or as a list with elements named 'x' and 'y'.  Probabilities (p or
##' p$y) should be non-negative; negative values will be silently set
##' to 0.
##' 
##' @param x The x-coordinates of the PDF.  If p is an atomic vector
##' and x is not given, x is assumed to be 1:length(p).  Values of x
##' must be strictly increasing, but need not be uniformly spaced.
##'
##' @param normalize Logical; if true, normalizes the PDF to integrate
##' exactly to 1.  (PDF estimates produced by base::density often
##' need this correction.)
##'
##' @param expand Logical; if true, returns a CDF two elements longer
##' than the input PDF that includes the two fencepost values added
##' onto the ends for integration.  If normalize=TRUE, the fenceposts 
##' will be identically 0 and 1.
##'
##' @return A list with components 'x' and 'y'.
##'
##' x: vector of x coordinates for the distribution.
##'
##' y: vector of probabilities that the distribution takes on a value
##'    less than or equal to the corresponding x.
##'
##' @examples
##' library(nor1mix)
##' r = rnorMix(1e4,norMix(c(-3,3)))
##' par(mfrow=c(2,1))
##' plot(density(r))
##' plot(pdf2cdf(density(r)),type='l')
##'
##' @export


pdf2cdf <- function(p, x=NULL, normalize=TRUE, expand=FALSE){
    if(is.null(x)) {
        if(is.atomic(p)){
            y <- p
            x <- seq(length(y))
        } else {
            stopifnot(exists("x", where=p) && exists("y", where=p))
            y <- p$y
            x <- p$x
        }
    } else {
        stopifnot(is.atomic(p) && is.atomic(x))
        y <- p
    }
    
    stopifnot(length(x)==length(y),
              length(x) > 2,
              all(is.finite(x)),
              all(is.finite(y)),
              all(diff(x) > 0)
              )

    if(any(y < 0)){
        y[y < 0] <- 0
    }
    
    ## trapezoids: N = no., L = left height, R = right height, W = width
    N <- length(y) + 1
    L <- c(0,y)
    R <- c(y,0)
    W <- diff(x)
    W <- c(W[1], W, W[N-2])
    area <- W * (L + R) / 2
    cdf <- cumsum(area)

    if(normalize){
        cdf <- cdf / cdf[N]
    }
    if(expand){
        return(list(x=c(x[1]-W[1],x,x[N-1]+W[N]), y=c(0,cdf)))
    } else {
        return(list(x=x, y=cdf[-N]))
    }
}

### Copyright 2015 Univ. Corp for Atmos. Research
### Author: Seth McGinnis, mcginnis@ucar.edu
