##' Overloaded infix @@ for attribute access
##' 
##' The at-sign, '@@', is normally used with S4 objects for slot
##' access.  These functions allow the use of infix @@ on non-S4
##' objects to get and set attributes.
##'
##' The right-hand-side of the @@ operator (the \code{name} argument)
##' need not be quoted.  It is converted to a literal character string
##' by computation on the language using \code{match.call}.  Attribute
##' names must be matched exactly.
##' 
##' These functions check whether their left-hand argument is an S4
##' object, and if so, pass it through to the \code{slot} function,
##' leaving the original use of @@ for slot access unaffected.
##'
##' @param object A non-S4 object.
##'
##' @param name The character-string name of the attribute to access,
##' quoted or not.
##'
##' @param value The value of the attribute to be set.
##'
##' @return The value of the attribute.
##'
##' @examples
##'
##' mu <- 3
##' sigma <- 2
##' x <- rnorm(10, mean=mu, sd=sigma)
##' 
##' attr(x, "mu") <- mu
##' print(attr(x, "mu"))
##' 
##' x@@sigma <- sigma
##' print(x@@sigma)
##' 
##' print(x)
##' x@@mu <- NULL
##' x@@mu_s <- mean(x)
##' x
##'
##' @seealso \code{\link{copyatts}}
##'
##' @importFrom methods slot slot<-
##' 
##' @name atsign

## Dummy object to make the documentation work
NULL

##' @rdname atsign
##' 
##' @usage object@@name
##'
##' @export

`@` <- function(object, name){
    name <- as.character(match.call()[["name"]])
    if(isS4(object)){
      slot(object, name)
    } else {
      attr(object, name, exact=TRUE)
    }
}


##' @rdname atsign
##' 
##' @usage object@@name <- value
##'
##' @export

`@<-` <- function(object, name, value){
    name <- as.character(match.call()[["name"]])
    if(isS4(object)){
      `slot<-`(object, name, value)
    } else {
      `attr<-`(object, name, value)
    }
}
