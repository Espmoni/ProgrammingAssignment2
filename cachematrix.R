## The functions create a special object that stores a matrix and caches its inverse. They allow to skip the time-consuming computations if the inverse has been already calculated.


##  makeCacheMatrix creates a special "matrix", which is really a list containing a function to set the values of the matrix, get the values of the matrix, set the values of the inversion, get the values of the inversion

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix function. Firstly, the function checks to see if the inverse has already been calculated. If so, it prints 'getting cashed data',gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the values of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


