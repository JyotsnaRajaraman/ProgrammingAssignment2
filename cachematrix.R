## Caching the Inverse of a Matrix:.
## there are two function below that can be used to 1. create a special object that 
## stores a matrix and caches the inverse.
## and then calculates the inverse, so if called again, doesn't have to recalculate

## makecache: creates the special matrix object to cache inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse)inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse= setInverse,
         getInverse = getInverse)
    

}


## computes the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    mat <- x$get()
    m <- inverse(mat, ...)
    x$setInverse(inv)
    inv
  
}
