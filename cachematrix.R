## The purpose of the functions below is to create a list object containing
## Function to store a matrix and inverse value without recalculating more than once.

## The makeCacheMatrix function creates a list of functions to use with a matrix
## you can set and/or get the value of the matrix (x)
## you can set and/or get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(z) {
    x <<- z
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the a makeCacheMatrix's matrix
## and stores it in the makeCacheMatrix object via setInverse function in order to
## cache the result avoiding the calculation for the next use of the inverse value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  d <- x$get()
  m <- solve(d, ...)
  x$setInverse(m)
}
