## Justin Owens
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Jan 24, 2018

## The following functions help eliminate redundant computations to calculate the inverse of a matrix.
## This proficiency is enabled by caching results the inverse of CacheMatrix object created from a matrix
## The cached result will then be retrieved instead of re-calculating the inverse

## Creates a cached matrix object with default null value for the inverse until cacheSolve() is called on the object
makeCacheMatrix <- function(x = matrix()) {
  s = NULL
  set = function(y) {
    x <<- y
    s <<- NULL
  }
  get = function() x
  setinverse = function(solve) s <<- solve
  getinverse = function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Obtains the cached inverse of matrix x or creates and stores the inverse matrix if not cached
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' from the cache or creates and caches a new result
  s = x$getinverse()
  if(!is.null(s)) {
    message("Obtaining cached data for inverse of matrix.")
    return(s)
  }
  data = x$get()
  s = solve(data)
  x$setinverse(s)
  s
}