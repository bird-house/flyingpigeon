##' Bias correction of climate model output using KDDM.
##'
##' This is the description of the function.  It's the first full
##' paragraph, and should briefly describe what the function does.
##'
##' Third and subsequent paragraphs are details: a long section shown
##' after the argument that goes into detail about how the function
##' works.
##'
##' @param bcdata The data to be bias-corrected.  A list with elements
##' named "obs", "cur", and "fut".
##'
##' @param norm The type of normalization to use.  See
##' \code{\link{normalize}} for more details.
##'
##' @param minobs The minimum number of observations to attempt a bias
##' correction.  If there are fewer that this many observations (e.g.,
##' for precipitaiton in a very dry location), non-zero model values
##' are set to NA.
##'
##' @param dmap Logical; if TRUE, returns the \code{\link{distmap}}
##' object generated during bias correction as an element named "dmap"
##' in the returned list.  N.B.: This option defaults to FALSE because
##' the distmap object is significantly larger than the inputs and
##' outputs of this function combined.
##' 
##' @param ... Arguments to pass to \code{\link{distmap}}.
##' 
##' @return The return value of the function.
##'
##' @examples
##' x <- "example code"
##' y <- "more example code"
##' paste(x, y)
##'
##' @importFrom stats predict
##' 
##' @export

biascorrect <- function(bcdata, norm="zscore", minobs=10, dmap=FALSE, ...){

  ## If there's too little data to estimate a PDF for either obs or
  ## cur, it's not possible to bias-correct.  Bail out and set the
  ## data values to NA instead, to indicate "the model had data here,
  ## but there's no way to tell what the correct values are."
  
  if(sum(is.finite(unlist(bcdata$obs))) < minobs |
     sum(is.finite(unlist(bcdata$cur))) < minobs ){
    unfixable <- function(x){
      x@uncorrectable <- is.finite(x)
      x + NA
    }
    
    result <- rapply(bcdata, unfixable, how="replace")
    if(dmap){ result$distmap <- distmap(c(0,1),c(0,1)) }    
    return(result)
  }

  
  ## normalize the three data components

    if(norm=="boxcox"){

      ## You can't really apply an adjustment to the denormalization
      ## for Box-Cox; you need to use the same parameter values you
      ## used for the obs or things go wonky.  This means that you
      ## also want to use the same params over all times in both cur
      ## and fut, so that you're not discarding any climate change
      ## signal when you normalize.  Given that, it makes sense to use
      ## a single value for all three, so we pool data to fit gamma to
      ## get the best compromise value.

      gamma <- megamma(unlist(bcdata, use.names=FALSE))
      nbcd <- rapply(bcdata, normalize, how="replace", norm=norm, gamma=gamma)
      
    } else {    
        nbcd <- rapply(bcdata, normalize, how="replace", norm=norm)
    }
    
    ## construct distribution mapping
    mapping <- distmap(unlist(nbcd$cur), unlist(nbcd$obs), ...)

    ## apply KDDM transfer function
    ## note: could skip predict(obs), but need its norm atts for next step
    fixed <- rapply(nbcd, function(x){predict(mapping, x)}, how="replace")

    ## extract & average normalization attributes
    adj <- rapply(fixed, attributes, how="replace")
    adj <- lapply(adj, renest)
    adj <- lapply(adj, function(x){lapply(x, function(y){unlist(y)})})
    adj <- lapply(adj, function(x){x$norm<-NULL; x})
    adj <- rapply(adj, mean, how="replace")
  ## Probably fix for entire month missing; need to test.
#    adj <- rapply(adj, mean, how="replace", na.rm=TRUE)
    adj <- renest(adj)

    ## calculate adjustments for denormalization
    shift <- NA
    scale <- NA
    pscale <- NA

    if(norm == "zscore"){
        shift  <- adj$mu$obs - adj$mu$cur
        scale  <- adj$sigma$obs   / adj$sigma$cur
    }

    if(norm == "boxcox"){
        shift <- 0
        pscale <- 1
    }

    ## denormalize bias-corrected data
    result <- list()
    result$obs <- bcdata$obs

    result$cur <- rapply(fixed$cur, denormalize, how="replace",
                         shift=shift, scale=scale, pscale=pscale)
    result$fut <- rapply(fixed$fut, denormalize, how="replace",
                         shift=shift, scale=scale, pscale=pscale)
    
    if(dmap){ result$distmap <- mapping }
    
    return(result)
}

