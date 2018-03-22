##' Calculates quantiles of one of the PDFs in a distmap object.
##'
##' This function does not use any of the quantile algorithms used by
##' the default quantile function.  Instead, it integrates the PDF in
##' the distmap object via the trapezoid rule to get a CDF, and then
##' inverts the CDF using splinefun.
##'
##' @param x An object of class 'distmap'.
##'
##' @param probs A vector of probabilities.  Values outside [0,1] are
##' set to NA.  Defaults to percentiles useful for plotting distmap
##' objects.
##' 
##' @param which The name of the pdf (xpdf or ypdf) to use for
##' quantiles.  Defaults to "xpdf".
##'
##' @param ... Further arguments passed from other functions.
##'
##' @return The return value of the function.
##'
##' @examples
##' set.seed(222)
##' x <- rnorm(1000)
##' y <- rnorm(1000, mean=3, sd=2)
##' d <- distmap(x, y)
##' quantile(d)
##' quantile(d, which="ypdf", prob=c(25,50,75)/100)
##'
##' @importFrom stats quantile
##' 
##' @export

quantile.distmap <- function(x, probs=c(5, 10, 25, 50, 75, 90, 95)/100, which=c("xpdf","ypdf"), ...){

    probs[probs < 0 | probs > 1] <- NA
    
    cdf <- pdf2cdf(x[[match.arg(which)]])
    q <- splinefun(cdf$y, cdf$x, method="monoH.FC")
    return(q(probs))
}

