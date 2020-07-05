## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. A pair of functions that cache the inverse of a matrix are
## written here below on the source file cacheMatrix.R


## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) xInv <<- inverseMatrix
  getInverse <- function() xInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInverse(xInv)
  xInv
  
}
