##' Remove precipitation zeros by setting them to NA
##'
##' \code{unzero} eliminates zero precipitation data from the bias
##' correction by setting zero values to NA, which is ignored.
##' \code{rezero} sets NA values back to zero after bias correction.
##'
##' The zeros in precipitation data cause problems for distribution
##' mapping.  Changing zeros to NA allows them to pass harmlessly
##' through the rest of the bias correction procedure without
##' requiring a lot of complicated and error-prone indexing to remove
##' and reinsert them from the arrays.
##'
##' It is not uncommon for precipitation data to have small negative
##' values after bias correction.  By default \code{rezero} also sets
##' those values to zero when returning NA values back to zero.  If
##' the input has an attribute named "uncorrectable", \code{rezero}
##' will use it to index into the array and set those values to NA
##' afterwards.  
##'
##' @param x A vector of numeric values
##'
##' @examples
##'
##' set.seed(22)
##' 
##' x <- rbinom(10, 2, 0.2)
##' y <- unzero(x)
##' x
##' y
##' rezero(y)
##' z <- unzero(c(x, NA, NA), warn=TRUE)
##' z <- c(z, -1)
##' z
##' rezero(z, refloor=FALSE)
##' 
##' @name unzero

## Dummy object to make the documentation work

NULL


##' @rdname unzero
##' 
##' @usage unzero(x, warn=FALSE)
##'
##' @param warn Logical; warn if NA values already present in input
##'
##' @export

unzero <- function(x, warn=FALSE, floor=TRUE){
    if(warn && any(is.na(x))){
        warning(sum(is.na(x)), " values already NA")
    }
    if(floor){
      x[x<=0] <- NA
    } else {
      x[x==0] <- NA
    }
    return(x)
}

##' @rdname unzero
##' 
##' @usage rezero(x, refloor=TRUE)
##'
##' @param refloor Logical; also set negative values to zero
##' 
##' @export

rezero <- function(x, refloor=TRUE){
    if(refloor){
        x <- pmax(x,0)
    }
    x[is.na(x)] <- 0
    x[x@uncorrectable] <- NA
    x@uncorrectable <- NULL
    return(x)
}
