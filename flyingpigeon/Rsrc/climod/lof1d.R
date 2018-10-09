##' Trims extreme values from a vector based on the local outlier
##' factor, simplified for the one-dimensional case.
##'
##' The local outlier factor (LOF) compares the local density of a
##' value to the average local density of its k nearest neighbors.  If
##' the ratio of the densities (the LOF) is significantly higher than
##' 1, the point is an outlier and can be discarded.  This function
##' uses a highly simplified calculation to trim outlier values from
##' the upper tail of 1-dimensional data.
##'
##' If length(x) is less than 3*k, the vector is returned unchanged.
##' See the Wikipedia page for a description of the LOF algorithm:
##' \url{https://en.wikipedia.org/wiki/Local_outlier_factor}
##'
##' @param x A vector of values.
##'
##' @param k The number of nearest neighbors to consider.  Also the
##' number of upper tail values considered as possible outliers.
##'
##' @param cutoff The maximum LOF for a value to be retained.
##' 
##' @return The input vector \code{x} with any of the \code{k} largest
##' values that have an LOF greater than \code{cutoff} removed.
##'
##' @examples
##' 
##' set.seed(222)
##' x <- c(-5, rnorm(20), 2, 5, 10, 50, 100)
##' print(x)
##' print(lof1d(x))
##' 
##' @export



lof1d <- function(x, k=5, cutoff=3){

  if(length(x) < 3*k){warning("lof1d: too few values to calculate 1-D LOF"); return(x)}
  
  ## To look at the top k values, we need the k nearest neighbors of the
  ## kth value, and then the k nearest neighbors of each of those, so we
  ## might need the 3*kth value.
  
  ox <- order(x, decreasing=TRUE)[1:(3*k)]  
  y <- x[ox]
  
  ## Find the k nearest neighbors for each value and their indices
  ## Note: k+1 index because A is an element of y, so first neighbor is self

  knni <- lapply(y, function(A){ order(abs(y-A))[(1:k)+1]})
  knn  <- lapply(knni, function(i){y[i]})

  ## K-distance = distance from A to its k-th nearest neighbor
  kdist <- abs(y-sapply(knn, `[`, k))

  ## Reachability distance of A from B = dist from A to B, but at least kd(B).
  ## Note!  This funciton operates on indices, not values
  rdist <- function(i,j){max(abs(y[i]-y[j]), kdist[j])}

  ## Local reachability density = inverse of avg rdist(A) *from* its knn
  N <- length(y)
  lrd <- sapply(seq(N), function(i){
    1/mean(sapply(knni[i], function(j){rdist(i,j)}))
  })

  ## Local outlier factor = avg knn lrd / own lrd
  lof <- sapply(1:N, function(i){mean(lrd[knni[[i]]]) / lrd[i]})
  
  ## trim values with LOF > cutoff

  if(any(lof > cutoff)){
    z <- x[-ox[lof > cutoff]]
  } else {
    z <- x
  }
    
  return(z)

}


