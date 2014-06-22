## function to create a special matrix containing our caching functions

makeCacheMatrix <- function(origMatrix = matrix()) {
  invMatrix <- NULL
  set <- function(mat) {
    origMatrix <<- mat
    invMatrix <<- NULL
  }
  get <- function() origMatrix
  setmean <- function(inverse) invMatrix <<- inverse
  getmean <- function() invMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
