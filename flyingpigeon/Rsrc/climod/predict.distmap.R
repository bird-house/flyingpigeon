##' Apply a distribution mapping transfer function to data
##'
##' Calls a distribution mapping (distmap) object's transfer function
##' on data.  This transforms the distribution of the data such that
##' x-distributed input will be y-distributed on output, for the x and
##' y datasets used to construct the mapping.
##'
##' @param object An object of class 'distmap'.
##'
##' @param xnew The data to be transformed.  If omitted, object$x is
##' used.
##'
##' @param ... Additional arguments to object$transfer(), which is
##' constructed by splinefun().
##'
##' @return A vector of transformed values.
##'
##' @examples
##' set.seed(222)
##' obs <- rgamma(1e6, shape=2)
##' mod <- rweibull(1e6,shape=3, scale=3)
##' mod2 <- exp(rnorm(1e6, sd=0.5)+1)
##' 
##' map <- distmap(mod, obs)
##' bc  <- predict(map)
##' bc2 <- predict(map, mod2)
##' 
##' plot(NA, type="n", xlim=c(0,10), ylim=c(0,0.5), main="bias-correction PDFs")
##' lines(density(obs),  col="black", lwd=3)
##' lines(density(mod),  col="red",  lwd=2)
##' lines(density(bc),   col="red",  lwd=2, lty=2)
##' lines(density(mod2), col="blue", lwd=2)
##' lines(density(bc2),  col="blue", lwd=2, lty=2)
##' legend("topright", c("obs","mod","bc","mod2","bc2"),
##'        col=c("black","red","red","blue","blue"), lty=c(1,1,2,1,2))
##'
##' @export


predict.distmap <- function(object, xnew=NULL, ...){
    if(is.null(xnew)){ xnew <- object$x }
    y <- xnew * NA
    y[!is.na(xnew)] <- object$transfer(xnew[!is.na(xnew)], ...)
    return(y)
}

### Copyright 2015 Univ. Corp for Atmos. Research
### Author: Seth McGinnis, mcginnis@ucar.edu
