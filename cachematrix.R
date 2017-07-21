# makeCacheMatrix:
# Creates a special "matrix" object that can cache its inverse
# Special "matrix" object is a list containing the closure functions:
# 1. set: sets the values of the matrix
# 2. get: gets matrix data from the special "matrix" object
# 3. getInv: gets the cached inverse of the matrix 
# 4. setInv: sets the cached inverse of the matrix

# cacheSolve:
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve retrieves the inverse from the cache.

# x is a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y){
    x <<- y # assigns value of y to x
    # Reset inv to NULL so because inv changes as well
    inv <<- NULL
  }
  getInv <- function() inv
  setInv <- function(inverse) inv <<- inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# Argument 'x' is the special matrix object defined above
cacheSolve <- function(x, ...) {
  # get cached value of inverse
  inv <- x$getInv()
  if (!is.null(inv)){
    # inverse has been caluclated
    message("getting cached data")
    return(inv)
  }
  # no cache record of inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

