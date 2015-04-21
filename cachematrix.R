## Matrix inversion is usually a costly computation and it is beneficial to cache
## the inverse of a matrix rather than compute it repeatedly.
## The following functions achieve that.
##
## Usage:
## a <- matrix(1:4, 2,2)
## c <- makeCacheMatrix(a)
## cacheSolve(c)
## cacheSolve(c)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
