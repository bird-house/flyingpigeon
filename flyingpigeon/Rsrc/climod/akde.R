##' This is a wrapper for the \code{\link[quantreg]{akj}} adaptive
##' kernel density estimation routine.
##'
##' This function calls \code{quantreg::akj} and formats its return
##' value as a list with x & y elements, consistent with other kernel
##' density estimation functions.  It also provides arguments for
##' defining the x-grid that the PDF is estimated on.
##'
##' The documentation for \code{akj} says that the data values are
##' assumed to be sorted.  Although it doesn't appear to matter in
##' practice, this function sorts them just in case.
##' 
##' @param x The data to estimate a PDF for.
##'
##' @param n The size of the x-grid to estimate the density on.
##' Defaults to twice the length of x, which is the default used by
##' \code{akj}.
##'
##' @param extend The distance beyond the range of the data to extend
##' the x-grid.  Defaults to 3 times the bandwidth calculated by
##' \code{\link[stats]{bw.nrd}}, which is close to the default values
##' used by both \code{\link[stats]{density}} and
##' \code{\link[KernSmooth]{bkde}}.
##'
##' @param min.x The minimum value of x to compute the density on.
##' Defaults to the minimum data value minus \code{extend}.
##'
##' @param max.x The maximum value of x to compute the density on.
##' Defaults to the maximum data value plus \code{extend}.
##' 
##' @return A list with following components:
##'
##' x: the x-values where the density is estimated.
##'
##' y: the estimated density values at each x.
##'
##' 
##'
##' @examples
##' library(KernSmooth)
##' set.seed(222)
##' x <- rgamma(300, shape=3, scale=2)
##' d <- namelist(density, bkde, akde)
##' p <- lapply(d, do.call, list(x=x))
##' mplot(p)
##' legend("topright", names(d), lty=seq(3), col=seq(3))
##'
##' @importFrom stats bw.nrd
##' @importFrom quantreg akj
##' 
##' @export

akde <- function(x,
                 n = 2*length(x),
                 extend = 3 * stats::bw.nrd(x),
                 min.x = min(x) - extend,
                 max.x = max(x) + extend){
    z <- seq(min.x, max.x, length=n)
    kde <- quantreg::akj(sort(x), z)
    return(list(x=z, y=kde$dens))
}
