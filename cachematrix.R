#Remove everything from Environment
rm(list = ls())

## Caching The Inverse Of A Matrix:
## Matrix inversion is usually a costly computation and there are some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## This is a function that can cache its inverse by creating a special matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This is a function that computes the inverse of the special matrix (created above).  However, there are conditions: if the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of x
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


