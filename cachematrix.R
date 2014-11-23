## this file allows for a cached retrieval of an inverted matrix.

## creates an object representing a matrix with an precomputed inverted matrix. 
makeCacheMatrix <- function(m = matrix()) {
  mi <- NULL
  set <- function(y) {
    m <<- y
    mi <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## computes the inverted matrix if it has not been cached yet. Returns the inverted matrix.
cacheSolve <- function(m, ...) {
  inverse <- m$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  thematrix <- m$get()
  inverse <- solve(thematrix, ...)
  m$setinverse(inverse)
  inverse
}
