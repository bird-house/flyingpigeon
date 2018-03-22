##' Trims the top percentile of values from a vector.
##'
##' Because precipitation is heavy-tailed, the process of
##' constructing a distribution mapping (a \code{\link{distmap}}
##' object) for precipitation tends to overfit the upper tails.  To
##' avoid this problem, it is useful in this case to trim extreme
##' values from the input datasets. This function discards the top
##' \code{p}% of values from the input vector.
##'
##' This function uses the \code{stats::quantile} function to find a
##' cutoff value.  Because any quantile with probability less than 1
##' is always less than the maximum value, the function will always
##' trim the maximum value from x.
##'
##' @param x a vector of values.
##'
##' @param p the percentage of values to discard.  Defaults to 1%.
##'
##' @param ... further arguments passed on to \code{stats::quantile}
##'
##' @return The input vector \code{x} with the top \code{p} percent
##' of values (minimum 1) removed.
##'
##' @examples
##' 
##' set.seed(222)
##' x <- c(rnorm(20), 2, 5, 10, 50, 100)
##' print(x)
##' print(ptrim(x, p=0.10))
##' print(quantile(x, p=0.9))
##' @export

ptrim <- function(x, p=0.01, ...){
  q <- quantile(x, probs=(1-p), ...)
  x[x < q]
}
