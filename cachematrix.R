## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.  These functions will 
## cache the inverse of a matrix.

## makeCacheMatrix function will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(b) {
    x <<- b
    a <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) a <<- solve
  getsolve <- function() a
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolv function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getsolve()
  if(!is.null(a)) {
    message("Getting cached data ...")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setsolve(a)
  a
}
