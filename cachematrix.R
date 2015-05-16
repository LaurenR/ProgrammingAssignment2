## These two functions create a matrix, then find the inverse of that matrix
## while caching the result. Once the result is cached, that result is retrieved 
## without going through the entire calculation all over again.

## makeCacheMatrix is a function that takes a matrix as an argument, then establishes the 
##cached variables, the first time it encounters a matrix, variables will be set to NULL

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve(x)
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is a funtion that takes the matrix as an argument and first searches
## the values in makeCacheMatrix for the already calculated inverse, if set to NULL
## cacheSolve calculates inverse and populates cache variables

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
