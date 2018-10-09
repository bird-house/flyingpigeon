##' Ingest all the variables in a netCDF file
##'
##' This function reads in all the variables in a netCDF file, names
##' their dimensions, and attaches attributes to them such that they
##' have approximately the same representation in R as they do in the
##' netCDF file.  If you're used to thinking about netCDF files, this
##' will make them easier to work with, because it will allow you to
##' write code like \code{data <- nc$tmax[,nc$lon<300,]} or
##' \code{if(nc$time@@calendar == "noleap")}
##'
##' This function reads in all variables and coordinate variables in a
##' netcdf file and returns them in a list.  It sets attributes on the
##' variables matching the netcdf attributes; these can be accessed
##' using the overloaded \code{\link{@@}} operator in this package.
##' 
##' Dimensions are named but not reordered to match the native netcdf
##' ordering, because that difference is caused by the difference in
##' how R and C order the most-rapidly-varying array indices, and
##' changing the ordering would be both time-consuming and
##' inefficient.  Degenerate dimensions are culled.
##'
##' There is currently no custom print method for the resulting object
##' of this function. For a view of the contents of a netcdf file
##' similar to what \code{ncdump -h} products, simply print the ncdf4
##' object produced by \code{nc_open}.
##' 
##' @param netcdf An ncdf4 object (the result of
##' \code{ncdf4::nc_open}), or the name of an existing netCDF file to
##' be opened and ingested.
##'
##' @param globalatts Logical; if true, reads in global attributes in
##' addition to data variables.  (Defaults to FALSE.)
##'
##' @return An object of class 'nc' containing the variables in the
##' netcdf file with named dimensions and attributes as in the netcdf
##' file.
##' 
##' @seealso \code{\link{atsign}}
##' 
##' @importFrom ncdf4 nc_open ncvar_get ncatt_get
##' 
##' @export

nc_ingest <- function(netcdf, globalatts=FALSE){

    stopifnot( class(netcdf)=="ncdf4" || is.character(netcdf))
    if(class(netcdf) == "ncdf4"){
        nc <- netcdf
    } else {
        nc <- nc_open(netcdf)
    }

    x <- list()

    dimvars <- c()

    ## read in data variables
    for(v in names(nc$var)){

        ## skip dummy vars
        if(nc$var[[v]]$ndims > 0){
      
            ## read variable
            var <- ncvar_get(nc, v, collapse_degen=TRUE)
      
            ## name dimensions
            dnames <- sapply(nc$var[[v]]$dim, function(x){x$name})
            ## cull degenerate dimensions
            dlen <- sapply(nc$var[[v]]$dim, function(x){x$len})
            dnames <- dnames[!(dlen == 1)]
            
            ## dimnames will take list of named NULL elements
            ## this names the dimensions but not rows/cols
            dimnames(var) <- sapply(dnames, function(x){NULL})
            dimvars <- c(dimvars, dnames)
        }        

        ## read in variable's attributes and attach to variable
        varatts <- ncatt_get(nc, v)

        for(a in names(varatts)){
            attr(var, a) <- varatts[[a]]
        }

        ## save to object
        x[[v]] <- var               
    }

    ## read in coordinate variables
    dimvars <- unique(dimvars)

    for(v in dimvars){
        if(nc$dim[[v]]$create_dimvar){
            ## read var
            var <- ncvar_get(nc, v)
            ## name dim (always 1D)
            dimnames(var) <- sapply(v, function(x){NULL})
            ## read in attributes and attach
            varatts <- ncatt_get(nc, v)

            for(a in names(varatts)){
              attr(var, a) <- varatts[[a]]
            }
            ## save to object
            x[[v]] <- var               
        }
    }
    
    if(globalatts){
        ## read in global attributes
        globalatts <- ncatt_get(nc, 0)
    
        for (a in names(globalatts)){
            attr(x,a) <- globalatts[[a]]
        }
    }

    class(x) <- "nc"
    return(x)
}
