##' Copy all attributes from one variable to another
##'
##' This function sets the attributes of the argument \code{to} to
##' match the attributes of \code{from}.  The special attributes
##' \code{dim}, \code{dimnames}, \code{names}, and \code{row.names}
##' are skipped to preserve dimensionality.  Existing attributes with
##' the same name are overwritten by default.
##'
##' Attribute ordering is mostly preserved.  After copying, special
##' attributes will come first, in alphabetical order.  Attributes of
##' the variable with priority (default: the variable being copied
##' from) come next, in their original order.  Any additional
##' attributes (default: the variable being copied to) come last,
##' again in their original order.
##'
##' This function exists because you can't simply assign
##' \code{attributes(x) <- attributes(y)} if x and y are arrays with
##' different dimensions.  You can avoid this problem by using
##' \code{mostattributes(x) <- attributes(y)} instead, but that
##' removes any dimensions on x, flattening it to a vector.  Like
##' \code{mostattributes<-}, it is principally intended for arrays,
##' and should be used with care on classed objects.
##'
##' @param from The variable to copy attributes from.
##'
##' @param to The variable whose attribute are to be set.
##'
##' @param clobber Logical: if true (default), from@@att will
##' overwrite to@@att.  If false, attributes on to take priority.
##' 
##' @return A copy of \code{to} with the attributes of \code{from}.
##'
##' @examples
##' 
##' x <- array(data=rnorm(120), dim=c(5,2,12))
##' x@dimnames <- list(lon=c(-92:-88), lat=c(45,46), time=month.abb)
##' x@name  <- "example"
##' x@units <- "none"
##' str(x)
##' 
##' y <- x[1:3,,1]
##' y@name <- "January subset"
##' y@month <- "Jan"
##' str(y)
##' 
##' z <- copyatts(x, y)
##' str(z)
##' 
##' w <- copyatts(x, y, clobber=FALSE)
##' str(w)
##' 
##' @export

copyatts <- function(from, to, clobber=TRUE){
    special <- c("dim", "dimnames", "names", "row.names")
    tatts <- attributes(to)
    fatts <- attributes(from)
    fatts[special] <- NULL
    satts <- tatts[special]
    tatts[special] <- NULL
    if (clobber){
        catts <- tatts
        catts[names(fatts)] <- fatts
    } else {
        catts <- fatts
        catts[names(tatts)] <- tatts
    }
    attributes(to) <- c(satts, catts)
    return(to)
}
