## cacheMatrix.R  - functions to return the inverse of a given matrix, storing the result in cache for faster 
##                  execution in a repeat situation.

## makeCacheMatrix - given a matrix argument, create a list of four functions to store and retrieve from the cache
##                   both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve - given a matrix, return its inverse. If given the same matrix again, retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!all(is.na(m))) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
