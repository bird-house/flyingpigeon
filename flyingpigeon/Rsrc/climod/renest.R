##' Invert the nesting of a list-of-lists
##'
##' Given a nested list, invert the nesting so that the inner list
##' becomes the outer list and vice-versa.  The structures of the
##' elements of the inner lists are preserved.  This function is more
##' easily demonstrated than explained; see examples.
##' 
##' All inner lists must have the same length.  The outer list of the
##' result will use the names of the first inner list (if they exist);
##' any other inner list names will be discarded.
##' 
##' @param lol a nested list.
##'
##' @return A nested list
##'
##' @examples
##'
##' f <- list(a=list(x=1, y=2, z=3), b=list(x="one", y="two", z="three"))
##' g <- list(x=list(a=1, b="one"), y=list(a=2, b="two"), z=list(a=3,b="three"))
##' 
##' 
##' print(str(f))
##' print(str(g))          
##' print(identical(g, renest(f)))
##' 
##' 
##' ## Inner list elements may themselves be lists or other complex
##' ## objects; they are left as-is.
##' 
##' i <- list(a=list(x=1, y=2, z=3),
##'           b=list(x=identity, y=seq(2), z=lapply(f,length)))
##' 
##'           
##' j <- list(x=list(a=1, b=identity),
##'           y=list(a=2, b=seq(2)),
##'           z=list(a=3, b=lapply(f,length)))
##' 
##' print(str(i))
##' print(str(j))          
##' print(identical(i, renest(j)))
##' 
##' 
##' ## Names of inner lists after the first are ignored and discarded.
##' 
##' h <- list(a=list(x=1, y=2, z=3),
##'           b=list(i="one", j="two", k="three"),
##'           c=list(z=2, y=4, x=6))
##' str(h)
##' str(renest(h))
##'
##' @export


renest <- function(lol) {
    if(length(unique(sapply(lol, length))) > 1){
        stop("inner lists have different lengths")
    }
    apply(do.call(rbind, lol), 2, as.list)
}




### Copyright 2015 Univ. Corp for Atmos. Research
### Author: Seth McGinnis, mcginnis@ucar.edu
