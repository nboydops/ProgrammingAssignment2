
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Invrs <- NULL
  set <- function(y) {
    x <<- y
    Invrs <<- NULL  
  }
  get <- function() x
  setInverse <- function(Inverse) Invrs <<- Inverse
  getInverse <- function() Invrs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve<- function(x, ...) {
  Invrs <- x$getInverse()
  if(!is.null(Invrs)) {
    message("getting cached data")
    return(Invrs)
  }
  data <- x$get()
  Invrs <- solve(data, ...)
  x$setInverse(Invrs)
  Invrs
}


