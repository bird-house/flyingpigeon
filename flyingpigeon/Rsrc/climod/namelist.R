##' Construct a list with automatic names
##'
##' Constructs a lists from its arguments, automatically naming each
##' element of the list with the name of the argument.
##'
##' @param ... objects to add to the list.
##'
##' @return A list with named elements
##'
##' @examples
##' x <- 3
##' y <- "a string"
##' z <- function(x){x^3 +4}
##' n <- namelist(x,y,z)
##' str(namelist)
##' 
##' @export

namelist <- function(...){
    result <- list(...)
    names(result) <- as.list(substitute(list(...)))[-1L]
    return(result)
}


### Copyright 2015 Univ. Corp for Atmos. Research
### Author: Seth McGinnis, mcginnis@ucar.edu
