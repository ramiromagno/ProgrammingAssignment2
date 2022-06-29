# Overview of the two functions:
#   - `makeCacheMatrix()`
#   - `cacheSolve()`
#
# `makeCacheMatrix()`: provides the infrastructure for caching the inverse of a
# matrix (using an approach based on `<<-` operator)
# `cacheSolve()`: either computes the inverse anew or retrieves it from the
# cache.
# 

# This function is analogous to the `makeVector()` function provided in the
# assignment example. Briefly, it generates an R object (a list) comprising four
# "methods" to operate on this object, meaning: getting and setting the matrix
# `x` and getting and setting the cached inverse of `x`.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function takes as input an object created by `makeCacheMatrix()` and
# returns the corresponding inverse matrix (no check is done on whether it is
# invertible or not). This is a straightforward adaptation of the `cachemean()`
# function given as example in the assignment.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
