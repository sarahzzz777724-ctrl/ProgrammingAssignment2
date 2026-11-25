## Matrix inversion can be costly, so these two functions cache the inverse
## of a matrix: once computed, it is stored and reused instead of recalculated.

## makeCacheMatrix: creates a special "matrix" object (a list of functions)
## that stores a matrix and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                  # cached inverse, empty at first
  set <- function(y) {
    x <<- y                                    # replace the matrix
    inv <<- NULL                               # matrix changed, so clear the old inverse
  }
  get <- function() x                          # return the matrix
  setinverse <- function(inverse) inv <<- inverse   # store the inverse in the cache
  getinverse <- function() inv                 # return the cached inverse (or NULL)
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: returns the inverse of the special "matrix" made by makeCacheMatrix.
## If the inverse is already cached (and the matrix has not changed), it returns
## the cached value; otherwise it computes the inverse with solve() and caches it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
