##' Trims extreme values from a vector that exceed Chauvenet's criterion.
##'
##' To prevent overfitting of the distributional tails when
##' constructing a \code{\link{distmap}} object, it is useful to trim
##' very extreme values from the input datasets.  This function trims
##' values whose probability is less than 1/N (greater than 1-1/N),
##' where N is the number of data points + 1/2, under the assumption that
##' the data is gaussian.
##'
##' The probability bounds are calculated using \code{qnorm}.  Instead
##' of using the sample mean and standard deviation, this function
##' uses the median and the MAD (median absolute deviation), which are
##' more robust against the effects of very extreme values.
##'
##'
##' @param x A vector of values to trim.
##'
##' @param upper Logical; trim values in the upper tail.  Defaults to TRUE.
##'
##' @param lower Logical; trim values in the lower tail.  Defaults to TRUE.
##'
##' @return A trimmed vector of values.
##'
##' @examples
##' x <- c(9, 10, 10, 10, 11, 50)
##' chauvenet(x)
##' chauvenet(c(x, -20), upper=FALSE)
##'
##' set.seed(222)
##' y <- c(rnorm(100))
##' range(y)
##' range(chauvenet(y))
##' z <- c(y, 100, 1000)
##' qnorm(1-1/12, mean(z), sd(z))
##' range(chauvenet(z))
##' @export

chauvenet <- function(x, upper=TRUE, lower=TRUE){

  n <- length(x) + 1/2
  mu <- median(x)
  sig <- mad(x)

  if(lower){ x <- x[!(x < qnorm(1/n, mu, sig))] }
  if(upper){ x <- x[!(x > qnorm(1-1/n, mu, sig))] }

  return(x)
}
