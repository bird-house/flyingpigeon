##' Plot multiple data series
##'
##' Given a list of objects, extracts an x-axis and a y-axis element
##' from each object, joins them together in a matrix, and passes them
##' to matplot.
##'
##' This is a convenience function used primarily for plotting
##' multiple PDFs (from, e.g., \code{stats::density} or
##' \code{KernSmooth::bkde}) on the same graph.
##' 
##' Each object must be a list with elements named "x" and "y", and
##' all x and y elements must be the same length.  (The elements are
##' merged into matrices via calls to sapply.)  These restrictions
##' make the function not terribly useful in the general case, but it
##' works fine on PDFs.
##'
##' @param list A list of objects, each with elements named 'x' and 'y'.
##'
##' @param x The name of the element to use for the X axis; defaults to "x".
##'
##' @param y The name of the element to use for the Y axis; defaults to "y".
##' 
##' @param ... Arguments to \code{matplot}
##'
##' @seealso \code{\link[graphics]{matplot}}
##'
##' @examples
##'
##' mu <- rnorm(5, sd=5)
##' sigma <- rgamma(5, shape=10, scale=0.25)
##' x <- mapply(function(m,s){rnorm(10000,m,s)}, mu, sigma, SIMPLIFY=FALSE)
##' d1 <- lapply(x, density)
##' d2 <- lapply(x, bkde)
##' ## Note that the length of x & y differ for bkde & density
##' d <- c(d1, d2)
##' mplot(d, type="l")
##'
##' @importFrom graphics matplot
##' 
##' @export


mplot <- function(list, x="x", y="y", ...){
    xdata <- as.matrix(lapply(list, '[[', x))
    ydata <- as.matrix(lapply(list, '[[', y))
    matplot(xdata, ydata, ...)
}
