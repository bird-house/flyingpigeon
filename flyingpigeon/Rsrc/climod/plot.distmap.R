##' Visualize a distribution mapping as a q-q plot with overlaid PDFs
##' and transfer function.
##' 
##' This function creates a Q-Q plot of the x and y elements of the
##' distmap object.  It overlays rug plots and pdf curves for the x and
##' y values, an identity line and a curve for the transfer function,
##' and dashed mapping lines running from the rug plots to the transfer
##' function, showing how x values are mapped through the transfer
##' function to the corresponding y values.
##'
##' This function is a composite of several other graphics functions,
##' which can be controlled by passing a list of arguments to the
##' function used to make that component of the plot.
##'
##' @param x An object of class 'distmap'.
##'
##' @param lim The range of the x and y axes.  By default, the plot is
##' set up to use the same range on both axes.
##'
##' @param pscale Vertical scaling of the PDF overlays, as a fraction
##' of the plot size (0 to 1).
##'
##' @param xmap A vector of x-values for the mapping lines.  Lines are
##' drawn from the values specified by xmap up to the transfer
##' function, then over to the y-axis.  Defaults to the (5, 10, 25,
##' 50, 75, 90, 95)th percentiles; \code{pretty(range(x))} can also be
##' informative.
##'
##' @param map.args A list of arguments to the segments() function,
##' which is used to draw the mapping lines.  Defaults to dashed red
##' lines.
##'
##' @param identity.args A list of arguments to abline(), which is used
##' to draw the identity line.
##'
##' @param rug.args A list of arguments to rug(), which is used to draw
##' rug plots along the axes.
##'
##' @param pdf.args A list of arguments to lines(), which is used to
##' draw the pdf curves.
##'
##' @param transfer.args  A list of arguments to lines(), which is used to
##' draw the transfer function.  Defaults to a thick red line.
##'
##' @param skip A vector of names of the plot components which are to
##' be skipped instead of plotted.  The names of the components are:
##' "identity", "xmap", "rug", "pdf", "transfer".  The base Q-Q plot
##' cannot be skipped.  Partial matching of names is allowed.
##'
##' @param xlim The range of the x-axis, if specified differently from
##' the y-axis.
##'
##' @param ylim The range of the y-axis, if specified differently from
##' the x-axis.
##'
##' @param pxscale Scaling of the PDF overlay for x-values, if
##' specified differently from the PDF for y-values.
##'
##' @param pyscale Scaling of the PDF overlay for y-values, if
##' specified differently from the PDF for x-values.
##'
##' @param pxlim The vertical range of the PDF overlay for x-values,
##' specified in data units along the y-axis.  (An alternative to
##' controlling the overlay height using pscale.)
##'
##' @param pylim The horizontal range of the PDF overlay for y-values,
##' specified in data units along the x-axis.  (An alternative to
##' controlling the overlay width using pscale.)
##'
##' @param ... Other arguments to qqplot(), which is used to set up the
##' base plot on which all other components are overlaid.
##' 
##' 
##' @examples
##' 
##' library(nor1mix)
##' set.seed(222)
##' 
##' x <- rnorMix(1000, norMix(mu=c(-3,2), sigma=c(2,1), w=c(1,2)))
##' y <- rnorMix(1200, norMix(mu=c(-2,2)))
##' 
##' dmap <- distmap(x,y)
##' plot(dmap)
##' 
##' dev.new()
##' tblack <- adjustcolor("black",0.1)
##'
##' plot(dmap, pscale=0.1, id=list(col="blue"),
##'      rug=list(col=tblack), pdf=list(col="blue",lty=3),
##'      skip=c("transfer", "xmap"), pxscale=0.3, pyscale=0.15,
##'      xlim=c(-10,10), pch=3, main="Example")
##'
##' 
##' @importFrom scales rescale
##' @importFrom graphics abline lines rug segments
##' @importFrom stats qqplot
##'
##' @export


plot.distmap <-
function(x,
         lim=range(c(x$xq, x$yq)),
         pscale=0.2,
         xmap=quantile(x),
         map.args=list(col="red", lty=2),
         identity.args=list(),
         rug.args=NULL,
         pdf.args=NULL,
         transfer.args=list(col="red", lwd=2),
         skip=NULL,
         xlim=lim,
         ylim=lim,
         pxscale=pscale,
         pyscale=pscale,
         pxlim=c(min(ylim), min(ylim) + diff(ylim) * pxscale),
         pylim=c(min(xlim), min(xlim) + diff(xlim) * pyscale),
         ...){

    object <- x
    
    # base Q-Q plot
    qqplot(object$x, object$y, xlim=xlim, ylim=ylim, ...)

    # identity line
    if(all(is.na(pmatch(skip,"identity")))){
        do.call(abline, c(list(a=0,b=1), identity.args))
    }

    # map lines
    if(all(is.na(pmatch(skip,"xmap")))){
        ymap <- object$transfer(xmap)
        do.call(segments, c(list(x0=min(xlim),x1=xmap,y0=ymap,y1=ymap), map.args))
        do.call(segments, c(list(y0=min(ylim),y1=ymap,x0=xmap,x1=xmap), map.args))
    }
            
    # rug plots of data values
    if(all(is.na(pmatch(skip,"rug")))){
        do.call(rug, c(list(x=object$x, side=1), rug.args))
        do.call(rug, c(list(x=object$y, side=2), rug.args))
    }
        
    # overlaid pdf curves on corresponding axes
    if(all(is.na(pmatch(skip,"pdf")))){
        xpx <- object$xpdf$x
        ypx <- object$ypdf$x
        xpy <- rescale(object$xpdf$y, pxlim)
        ypy <- rescale(object$ypdf$y, pylim)
        do.call(lines, c(list(x=xpx, y=xpy), pdf.args))
        do.call(lines, c(list(x=ypy, y=ypx), pdf.args))
    }
        
    # transfer function line
    if(all(is.na(pmatch(skip,"transfer")))){
        do.call(lines, c(list(x=object$xq, y=object$yq), transfer.args))
    }
}


### Copyright 2015 Univ. Corp for Atmos. Research
### Author: Seth McGinnis, mcginnis@ucar.edu

