## cachematrix.R: this file implements functions for a matrix with a cacheable
## inverse operation. Submission by https://github.com/OhmNomNom

## makeCacheMatrix: this function creates a cacheMatrix from the first
## parameter, defaulting to an empty matrix.
##
## The cacheMatrix has the following methods:
##
## cacheMatrix$get(): gets the contents as a regular matrix
## cacheMatrix$set(y): sets the contents to the matrix y, resetting the cache
##
## PRIVATE FUNCTIONS BELOW! use cacheSolve to get the inverse of cacheMatrix
##
## cacheMatrix$p.getInverse(): gets the cached inverse; NULL if no inverse is set
## cacheMatrix$p.setInverse(new_inv): set the inverse cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(new_inv) inv <<- new_inv
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       p.setInverse = setInverse,
       p.getInverse = getInverse)
}


## This returns the inverse of the cacheMatrix. As long as the matrix has 
## not been changed between calls, successive calls to cacheSolve(m) 
## would not result in re-computation of the inverse (using the intrinsic)
## cache.
##
## Note: all parameters other than x (the cacheMatrix) are ignored given they
## have not been mentioned in the assignment. If we pass them to solve(),
## then we need to check when we retrieve the cache whether they had been 
## the same when the cache was first computed

cacheSolve <- function(x, ...) {
  res <- x$p.getInverse()
  if(is.null(res)) {
    res <- solve(x$get())
    x$p.setInverse(res)
  }
  res
}
