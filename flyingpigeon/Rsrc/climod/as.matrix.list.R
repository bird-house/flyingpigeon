##' Converts a list to a matrix, padding vectors with NA to fit.
##'
##' This function cbinds the elements of a list together into a
##' matrix.  Elements shorter than the longest element are padded with
##' NA values to fill out the matrix.  If the elments themselves are
##' lists, a specific sub-element can be extracted by name using the
##' \code{element} argument.
##'
##' This is a convenience function intended primarily to make it easy
##' to overplot a list of similar objects all at once using
##' \code{matplot}.  The elements are padded out to a length of N by
##' indexing them from 1:N; this works sensibly for vectors, but may
##' produce unexpected results if the (sub-) elements of your list are
##' some other kind of object.
##'
##' @param x list to be converted to a matrix
##'
##' @param element sub-element of each list to extract before
##' converting to matrix
##'
##' @param ... additional arguments (ignored; included for compatibility)
##' 
##' @return a matrix
##'
##' @examples
##' a <- seq(5,10)
##' b <- rev(a)
##' N <- a * 100
##' x <- mapply(function(f,g){f + seq(g)/g}, a, N)
##' y <- mapply(rbeta, N, a, b)
##' z <- lapply(y, sort)
##' 
##' str(y)
##' 
##' matplot(as.matrix(x), as.matrix(z), pch=1)
##' 
##' d <- lapply(y, density, na.rm=TRUE)
##' matplot(as.matrix(d, "x"), as.matrix(d, "y"), type='l')
##' 
##' @export

as.matrix.list <- function(x, element=NULL, ...){
    if(!(is.null(element))){
        x <- lapply(x, `[[`, element)
    }
    n <- max(sapply(x, length))
    do.call(cbind, lapply(x, `[`, 1:n))
}
