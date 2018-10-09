##' Climatological slicing of timeseries
##'
##' \code{slice}, \code{subslice}, and \code{unslice} are used to
##' separate a timeseries into climatological windows, then
##' reconstruct the timeseries from the windowed data.
##'
##' \code{slice} uses a \code{\link{cslice}} object to separate a
##' timeseries into climatological windows.  \code{unslice}
##' reconstructs the timeseries from the (possibly modified) sliced
##' data.  \code{subslice} takes data that has been sliced into outer
##' windows and extracts the inner windows from each slice.
##'
##' Whether the sliced data is further segmented by year depends on
##' whether the \code{cslice} object is segmented (i.e., called with
##' \code{split=TRUE}).  The \code{subslice} function also accepts a
##' \code{split} argument to override the default.
##' 
##' @param x A vector of values to be sliced into windows.
##'
##' @param s Sliced data -- a list of vectors, each corresponding to
##' a different climatological window.
##'
##' @param how A \code{cslice} object defining the windows to use for
##' slicing.
##'
##' @param outer Logical: if TRUE, uses the outer windows from
##' \code{how}, which can overlap.  If FALSE (the default), uses inner
##' windows, which cover the data completely without overlapping.
##'
##' @param split Whether to segment subsliced data by year.  Defaults
##' to the \code{split} parameter of \code{how}.
##' 
##' @name slice

## Dummy object to make the documentation work

NULL


##' @rdname slice
##' @usage slice(x, how, outer=FALSE)
##'
##' @return a list of vectors, one for each climatological window.
##' 
##' @export

slice <- function(x, how, outer=FALSE){
    if(outer){
        index <- how$outer
    } else {
        index <- how$inner
    }
    rapply(index, function(i){return(x[i])}, how="replace")
}


##' @rdname slice
##' @usage subslice(s, how, split=how$params$split)
##'
##' @return a list of vectors, one for each climatological window.
##' 
##' @export

## Subslice gets a split argument to override the default because its
## most common use case is being called as part of unslice.  Since
## unslice needs the inner slice to be unsegmented, it would be
## pointless to desegementize the data in order to subslice, resegment
## it to return, and then immediately desegmentize it again in the
## calling function.

subslice <- function(s, how, split=how$params$split){
    ## Desegmentize to subslice, then resegmentize if needed
    iind <- lapply(how$inner, unlist)
    oind <- lapply(how$outer, unlist)
    inout <- mapply(match, iind, oind, SIMPLIFY=FALSE)
    s <- mapply('[', lapply(s, unlist), inout, SIMPLIFY=FALSE)
    if(split){
        ibdy <- lapply(iind, function(x){which(diff(x) > 1)})
        for(i in 1:length(s)){
            s[[i]] <- mapply(function(a,b){iind[a:b]}, SIMPLIFY=FALSE,
                             c(0,ibdy)+1, c(ibdy,length(s)))
        }
    }
    return(s)
}


##' @rdname slice
##' @usage unslice(s, how)
##' 
##' @return a single timeseries vector.
##' 
##' @export

unslice <- function(s, how){

    ## Need desegmentized inner
    iind <- lapply(how$inner, unlist)
    
    ## if s in an outer slice, convert it to an inner slice
    if(identical(rapply(s, length, how="replace"),
                 rapply(how$outer, length, how="replace"))){
        s <- subslice(s, how, split=FALSE)
    }
    if(!identical(sapply(s, length), sapply(iind, length))){
        stop("Sliced data lengths don't match cslice object")
    }
    result <- c()
    for(i in 1:length(s)){
        result[iind[[i]]] <- s[[i]]
    }
    return(result)
}

