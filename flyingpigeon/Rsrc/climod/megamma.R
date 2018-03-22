##' Find the maximum-entropy value of the gamma (shift) parameter for
##' the Box-Cox transform with lambda=0.
##'
##' The Box-Cox transform is often used to normalize precipitation
##' data.  In practice, the best value of the lambda parameter (the
##' exponent) is usually zero, but the best value of the gamma (shift)
##' parameter is unpredictable.  This function finds the best value of
##' gamma by maximizing the entropy of the PDF of the transformed
##' data.  This works because (of all distribution on the real line
##' with a given variance) the normal distribution has the maximum
##' possible entropy, which is calculated as minus the integral of
##' \code{p log p}.  Entropy can therefore be used as a measure of how
##' close a distribution is to normal.
##'
##' This function calculates entropy by estimating the PDF of the
##' (transformed) data using \code{KernSmooth::bkde} and summing
##' \code{-p log p} of the result.  Note that because \code{bkde} uses
##' a fixed number of bins and uses the variance of the data to scale
##' the gaussian kernel to fit the range of the data, there is no need
##' to explicitly rescale the data to have unit variance or to divide
##' the sum by the bin width; these factors have already been taken
##' into account by the kernel density estimation.  This function uses
##' \code{stats::optimize} to find the maximum-entropy value of gamma.
##' A more general optimization can be done over both gamma and lambda
##' using \code{optim} instead, but it is much, much slower, and tests
##' on real data showed that the lambda parameter strongly tends
##' towards zero in all cases except when the number of data points is
##' very small.
##'
##' @param x A vector of values
##'
##' @return The maximum entropy value of gamma
##'
##' @examples
##'
##'
##' 
##' library(KernSmooth)
##' set.seed(222)
##' x <- rgamma(1000, shape=3, rate=2)
##' 
##' plot(bkde(x))
##' 
##' g <- megamma(x)
##' 
##' y <- log(x + g)
##' 
##' plot(bkde(y))
##' lines(bkde(log(x)), col="red")
##' lines(bkde(log(x+2*g)), col="blue")
##'
##' @importFrom KernSmooth bkde
##' @importFrom stats optimize
##'
##' @export

megamma <- function(x){
    x <- x[is.finite(x)]
    
    opt <- optimize(f=function(gamma, x){
        p <- bkde(log(x+gamma))
        p <- p$y[p$y > 0]
        -sum(p*log(p))
    },
                    x,
                    interval=c(-min(x), max(x)),
                    maximum=TRUE )
    return(opt$maximum)    
}
