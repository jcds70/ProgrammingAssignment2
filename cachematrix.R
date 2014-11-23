## makeCacheMatrix and cacheSolve are functions that allow
## caching the inverse of a matrix, so that it does not have
## to be repeatedly recomputed if it will be used in multiple
## calculations

## makeCacheMatrix is the cacheable matrix object, it
## has functions to get/set the value of the matrix and
## get/set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the inverse of the passed-in makeCacheMatrix
## object and stores it in cache if it has not been solved before.
## If the matrix has been solved before, cacheSolve
## returns the result from the cache instead of recomputing

cacheSolve <- function(x, ...) {
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}
