##' Adds a date-stamped comment to a NetCDF file's "history" attribute.
##'
##' It's good practice to add a comment to a NetCDF file's "history"
##' attribute whenever the file's contents are changed.  This is a
##' convenience function that makes it easy to do so from R.
##'
##' If a history attribute already exists, the comment is prepended to
##' it and separated by a carriage return.  A datestamp (produced by
##' \code{date()}) is added to the beginning of the comment.
##'
##' @param nc An object of class 'ncdf4' (as returned by 'nc_open')
##' corresponding to the file whose history is to be updated.
##'
##' @param comment The string to add to the history attribute.
##'
##' @importFrom ncdf4 ncatt_get ncatt_put
##' 
##' @export

nc_history <- function(nc, comment){
    hatt <- ncatt_get(nc,0,"history")
    history <- paste0(date(), ": ", comment)
    if(hatt$hasatt){
        history <- paste0(history, "\n", hatt$value)
    }
    ncatt_put(nc, 0, "history", history)
    NULL
}


## file.copy(Sys.glob("tests/raw/orog*"), "tests", overwrite=TRUE, copy.mode=FALSE)
## 
## f <- nc_open("tests/orog.hist.nc", write=TRUE)
## ncatt_get(f, 0, "history")
## 
## nc_history(f, "Added test history comment via nc_history")
## ncatt_get(f, 0, "history")
## 
## f <- nc_open("tests/orog.nohist.nc", write=TRUE)
## ncatt_get(f, 0, "history")
## 
## nc_history(f, "Added test history comment via nc_history")
## ncatt_get(f, 0, "history")
