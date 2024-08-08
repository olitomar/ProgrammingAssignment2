## Put comments here that give an overall description of what your
## functions do

## this function creates a special "vector", which is really a list containing a function to set and get the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function recalls the cached value, if not there, calculates it and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- inv$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- inv$get()
  inv <- solve(data, ...)
  inv$setinv(inv)
  inv
}
