## The following two functions provide a way to optimize computations where the inverse of a matrix is 
## repeatedly needed by caching the result of the first call for subsequent use.

## This function creates a wrapper for a matrix that allows caching of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of a wrapped matrix.
## On first call the inverse value will be cached, any subsequent calls will return the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
