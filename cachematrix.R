## Caching the Inverse of a Matrix:
## This functition is for creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-function(y){
    x<<-y
    m<<- NULL
  }
  get<-function() x
  setInverse <- function(inver) inverse <<- inver
  getInverse <- function()inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Calculate the inverse of the special 'matrix' created by the above function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}

