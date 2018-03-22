suppressMessages(library(KernSmooth))
suppressMessages(library(ncdf4))
suppressMessages(library(quantreg))

bc_kddm <- function(varname, obs, cur, fut, cout, fout, saveslice){

    isprec <- (varname == "prec" || varname == "pr")

    infiles   <- c()
    outfiles  <- c()

    infiles["obs"] <- obs
    infiles["cur"] <- cur
    infiles["fut"] <- fut

    outfiles["cur"] <- cout
    outfiles["fut"] <- fout


    if(isprec){
      norm <- "boxcox"
    } else {
      norm <- "zscore"
    }


    print(basename(outfiles["fut"]))

    nc <- lapply(infiles, nc_ingest)

    ## extract variables of interest from the netcdf objects
    indata <- lapply(nc,"[[",varname)

    ## if inputs are bad (all NA or obs always 0), output is all NA
    if(any(sapply(indata, function(x){all(!is.finite(x))})) ||
       all(indata$obs == 0)){

      for(i in c("cur","fut")){
        file.copy(infiles[i], outfiles[i], overwrite=TRUE, copy.mode=FALSE)
        fout <- nc_open(outfiles[i], write=TRUE)
        ncvar_put(fout, varname, ncvar_get(fout,varname)*NA)
        nc_close(fout)
      }

      quit(save="no", status=0, runLast=FALSE)
    }


    ## Normally there's a time coordinate variable in each file.  But if
    ## the data has been sharded for parallelism, it gets pulled out into
    ## its own file that lives alongside all the shards.  Here we attempt
    ## to determine which is the case and automagically do the right
    ## thing.

    time <- lapply(nc, "[[", "time")

    ## When sharded, the timefile is named "time.nc"
    timefiles <- paste(sapply(infiles, dirname), "time.nc", sep="/")
    names(timefiles) <- names(infiles)

    for(t in names(time)){
      if(is.null(time[[t]])){
        ## Note: we do this stuff by hand instead of using nc_ingest
        ## because the time files have no non-dimension vars in them.
        timenc <- nc_open(timefiles[t])
        time[[t]] <- ncvar_get(timenc, "time")
        time[[t]]@units <- ncatt_get(timenc, "time", "units")$value
        time[[t]]@calendar <- ncatt_get(timenc, "time", "calendar")$value
      }
    }


    ### Need to match up units and epochs
    time <- lapply(time, alignepochs, "days since 1950-01-01")


    ### data structures for storage of results
    outdata <- lapply(indata,"+",NA)

    ## width of moving window
    mwinwidth = 30


    ### generate climatology moving window index arrays
    ### 360 ~1-day moving windows w/ 30-day outer pool

    ### (360 is important because we want to be consistent from model to
    ### model and HadGEM uses a 360-day calendar, which will give you
    ### empty days if you use 365 windows.)

    ### Temperature data needs to be segemented into years and each year
    ### de/normalized separately to handle the trend.  Precip data needs
    ### to be pooled across years for dedrizzling and to ensure there are
    ### enough non-zero values to be able to fit the boxcox normalization.

    cwin <- lapply(time, cslice, outer=mwinwidth, num=360, split=!isprec)


    ##############################
    ## Do all the bias corrections
    nxy <- dim(indata$obs)[1:2]
    nx <- nxy[1]
    ny <- nxy[2]

    for(x in 1:nx){
        for(y in 1:ny){
            ## extract data for this gridcell
            data <- lapply(indata, function(a){a[x,y,]})

            ## window data using outer window
            wind <- mapply(slice, data, cwin,
                           MoreArgs=list(outer=TRUE), SIMPLIFY=FALSE)



            if(isprec){

              ## dedrizzle by slice
              ddz <- renest(lapply(renest(wind), dedrizzle))
              ## unzero
              ddu <- rapply(ddz, unzero, how="replace")
              ## dummy list on innermost nesting to match segmented slice structure for temp
              datatobc <- rapply(renest(ddu), list, how="replace")

              bctrunc <- TRUE
              bctrim  <- climod::ptrim
              densfun <- akde
            } else {
              ## just invert list nesting
              datatobc <- renest(wind)

              bctrunc <- FALSE
              bctrim  <- NULL
              densfun <- bkde
            }


            ## bias-correct each window

            fixdata <- lapply(datatobc, biascorrect, norm, dmap=saveslice,
                              truncate=bctrunc, trim=bctrim, densfun=densfun)


            if(saveslice){
              ## Have to separate out the distmaps, or unslicing breaks
              fixdata <- renest(fixdata)
              dmaps <- fixdata$distmap
              fixdata$distmap <- NULL
              fixdata <- renest(fixdata)
            }


            ## rearrange back to sliced conformation
            if(isprec){
              ## rezero data, get rid of the dummy inner list, and re-invert
              rez <- rapply(fixdata, rezero, how="replace")
              bc <- renest(lapply(rez, function(x){lapply(x, unlist)}))
            } else {
              ## just re-invert
              bc <- renest(fixdata)
            }


            ## collate inner windows back into timeseries
            result <- mapply(unslice, bc, cwin, SIMPLIFY=FALSE)

            ## save results in array
            for(i in names(result)){
                outdata[[i]][x,y,] <- result[[i]]
            }

            ## Save distmaps and data at peak slices
            if(saveslice){

              oraw <- lapply(wind, function(x){lapply(x, unlist)})
              iraw <- mapply(subslice, wind, cwin, split=FALSE, SIMPLIFY=FALSE)
              ifix <- mapply(subslice, bc,   cwin, split=FALSE, SIMPLIFY=FALSE)

              peaks <- c()
              peaks["obsmin"]   <- which.min(sapply(oraw$obs, mean, na.rm=TRUE))
              peaks["obsmax"]   <- which.max(sapply(oraw$obs, mean, na.rm=TRUE))
              peaks["rawcurpk"] <- which.max(sapply(iraw$cur, max,  na.rm=TRUE))
              peaks["rawfutpk"] <- which.max(sapply(iraw$fut, max,  na.rm=TRUE))
              peaks["fixcurpk"] <- which.max(sapply(ifix$cur, max,  na.rm=TRUE))
              peaks["fixfutpk"] <- which.max(sapply(ifix$fut, max,  na.rm=TRUE))

              jdays <- round(peaks/length(fixdata)*365.25)
              dates <- format(as.Date(jdays, origin="1950-01-1"), format="%b %d")

              dmaps <- dmaps[peaks]
              names(dmaps) <- names(peaks)

              ## save times (as fractional year) for outer segmented data

              toutseg <- mapply(slice, time, cwin,
                                MoreArgs=list(outer=TRUE), SIMPLIFY=FALSE)

              ## convert time to year (
              for(i in names(toutseg)){
                toutseg[[i]] <- rapply(toutseg[[i]], how="replace",
                                       function(x){x/yearlength(time[[i]]@calendar)+1950})
              }

              tslice <- renest(toutseg)[peaks]; names(tslice) <- names(peaks)


              ## not needed:
              ##  raw outer continuous <- renest(oraw)[peaks]
              ##  fix outer continuous <- renest(ofix)[peaks]

              ## (inner segemented doesn't really make sense)

              icraw <- renest(iraw)[peaks]; names(icraw) <- names(peaks)
              icfix <- renest(ifix)[peaks]; names(icfix) <- names(peaks)

              osraw <- renest(wind)[peaks]; names(osraw) <- names(peaks)
              osfix <- renest(bc)[peaks]  ; names(osfix) <- names(peaks)

              innerdata <- list(raw=icraw, fix=icfix)
              outerdata <- list(raw=osraw, fix=osfix)

              units <- indata$obs@units

              ## annual maxima
              iodata <- c(indata, outdata)
              iodata[4] <- NULL
              names(iodata) <- c("obs", "rawcur", "rawfut", "fixcur", "fixfut")
              iotime <- c(time,time)
              iotime[4] <- NULL
              names(iotime) <- c("obs", "rawcur", "rawfut", "fixcur", "fixfut")
              year <- lapply(iotime, function(x){floor(x/yearlength(x))})
              ydata <- mapply(split, iodata, year)
              bmax <- lapply(ydata, function(x){sapply(x, max, na.rm=TRUE, USE.NAMES=FALSE)})
              yyear <- lapply(year, function(x){unique(x)+1950})
              annmax <- lapply(mapply(cbind, yyear, bmax), as.data.frame)
              annmax <- lapply(annmax, `colnames<-`, c("year",varname))


              save(peaks, jdays, dates, dmaps, innerdata, outerdata, tslice, units,
                   annmax, file=savefile)
            }
        }
    }




    ## write results out to netcdf file

    for(i in c("cur","fut")){

      file.copy(infiles[i], outfiles[i], overwrite=TRUE, copy.mode=FALSE)

      fout <- nc_open(outfiles[i], write=TRUE)

      ncvar_put(fout, varname, outdata[[i]])

      ## bias correction metadata gets added to the final result, not
      ## intermediate output

      nc_close(fout)
    }
}
