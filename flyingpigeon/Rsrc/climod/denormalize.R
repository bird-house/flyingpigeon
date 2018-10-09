utils::globalVariables(c("norm","mu","sigma","lambda","gamma"))
## The overloaded use of @ involves non-standard evaluation that makes
## devtools::check() (or rather, codetools::checkUsagePackage()) think
## that the attribute names are undefined global variables.  Declaring
## them to be globalVariables suppresses the NOTE that would otherwise
## result.  (It actually doesn't complain about "norm", because
## base::norm() exists, but I list all the attributes here to make it
## clearer what's going on.)

##' Undo the normalization of a vector of values
##'
##' Reverses the transformation applied to a vector of values by the
##' function \code{normalize}, using the type and parameters of the
##' transformation as recorded in the vector's attributes.  See
##' \code{\link{normalize}} for details on the transformations.
##'
##' The ability to undo a normalization is useful in bias correction
##' of climate model output.  A bias correction will adjust the values
##' of the climate model output to match the statistical distribution
##' of observational data for a historical period, and then apply the
##' corresponding adjustment to model output for a future period.
##' Often, the performance of the bias correction will be improved if
##' the various inputs are first normalized in some way.  This
##' requires the normalization to be reversed after the bias
##' correction has been applied.
##'
##' When reversing the normalization, correction factors can be
##' supplied to adjust the normalization parameters to compensate for
##' bias.  The \code{shift} and \code{scale} arguments apply additive
##' and multiplicative adjustments, respectively, to the data.  The
##' \code{pscale} argument applies a multiplicative adjustment to the
##' exponent used in power normalization.
##'
##' The "power" transformation raises the data to an arbitrary power.
##' When undoing this transformation, the data is first floored at
##' zero to avoid problems with negative inputs.
##' 
##' @param x A normalized vector
##' 
##' @param shift An adjustment factor added to the data during
##' denormalization.  Adjusts mu or gamma, respectively, for "zscore"
##' and "boxcox" normalization.
##'
##' @param scale A multiplicative adjustment factor applied to the
##' denormalized data.  Only used for the "zscore" normalization.
##' Adjusts the variance of zscore-normalized data.
##'
##' @param pscale A multiplicative adjustment factor applied to the
##' exponent when denormalizing boxcox-transformed data.
##' 
##' @examples
##'
##' obs <- rgamma(10000, shape=5, scale=3)
##' cur <- rgamma(10000, shape=6, scale=2)
##' fut <- rgamma(10000, shape=3, scale=4)
##' 
##' data <- namelist(obs, cur, fut)
##' ndata <- lapply(data, normalize, norm="power")
##' 
##' dmap <- distmap(ndata$cur, ndata$obs)
##' ndata$bcc <- predict(dmap, ndata$cur)
##' ndata$bcf <- predict(dmap, ndata$fut)
##' 
##' par(mfrow=c(2,1))
##' 
##' N <- length(ndata)
##' mplot(lapply(ndata,density), type="l")
##' legend("topleft",names(ndata),lwd=1,lty=seq(N),col=seq(N))
##' 
##' denorm <- lapply(ndata[1:3], denormalize)
##' adjust <- ndata$obs@lambda / ndata$cur@lambda
##' denorm$bcc <- denormalize(ndata$bcc, scale=adjust)
##' denorm$bcf <- denormalize(ndata$bcf, scale=adjust)
##' 
##' N <- length(denorm)
##' mplot(lapply(denorm,density), type="l")
##' legend("topright",names(denorm),lwd=1,lty=seq(N),col=seq(N)) 
##'
##' @seealso \code{\link{normalize}}
##'
##' @export


denormalize <- function(x, shift=0, scale=1, pscale=1){

    norm <- x@norm
    
    if(!norm %in% c("zscore", "boxcox", "identity")){
        stop(paste("unknown normalization",norm))
    }
     
    if(norm == "zscore"){

        out.mu    <- x@mu    + shift
        out.sigma <- x@sigma * scale
           
        result <- x * out.sigma + out.mu
        result@mu <- NULL
        result@sigma   <- NULL
    }

    if(norm=="boxcox"){

        out.gamma  <- x@gamma + shift
        out.lambda <- x@lambda * pscale
        
        if (out.lambda == 0){
          result <- exp(x)
        } else {
          ## floor to avoid NaN (or worse, if 1/power is even...)
          x <- pmax(x,0)
          result <- (out.lambda * x + 1)^(1/out.lambda)
        }
        result <- result - out.gamma
	result@gamma  <- NULL
	result@lambda <- NULL
    }

    if(norm=="identity"){
        result <- x
    }
        
    result@norm <- NULL
    return(result)  
}


### Copyright 2015 Univ. Corp for Atmos. Research
### Author: Seth McGinnis, mcginnis@ucar.edu
