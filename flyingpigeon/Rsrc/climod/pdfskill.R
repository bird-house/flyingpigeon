##' Calculates the overlap between two PDFs.
##'
##' Given two datasets (observations and model), this function
##' calculates a skilll score as the area of overlap between their
##' estimated PDFs.  This metric comes from Perkins, et al. 2007 in
##' the Journal of Climate (doi: 10.1175/JCLI4253.1).
##'
##' This implementation uses binned kernel density estimation from the
##' KernSmooth package to estimate the PDFs.
##'
##' @param obs A vector of observed data.
##'
##' @param mod A vector of model data.
##'
##' @param ... Arguments to the density estimation function.
##'
##' @return A value between 0 and 1.
##'
##' @examples
##' obs <- rnorm(1000, mean=0, sd=1)
##' mod <- rnorm(1000, mean=0.3, sd=0.7)
##' pdfskill(obs, mod)
##'
##' @importFrom KernSmooth bkde
##' @importFrom stats na.omit
##' 
##' @export


pdfskill <- function(obs, mod, ...){
    obs <- na.omit(obs)
    mod <- na.omit(mod)
    range.x <- range(bkde(c(obs, mod), ...)$x)
    obspdf <- bkde(obs, range.x=range.x)
    modpdf <- bkde(mod, range.x=range.x)
    score <- sum(pmin(obspdf$y, modpdf$y))
    score <- score * diff(range.x) / (length(obspdf$x)-1)
    return(score)
}

