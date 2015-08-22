## cachematrix library
## completely derived from example code posted to cache the mean of a numeric vector
## by rdpeng as shown in README.md

## Creates a matrix object that allows for the caching of the inverse
## Initial constructor function for the cache matrix, takes a normal matrix as argument
makeCacheMatrix <- function(x = matrix()) {
  ## initialises the cache for the inverse to NULL
  s <- NULL
  ## getter and setter functions initialised for the matrix data
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  ## getter and setter functions initialised for the inverted matrix
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  ## returning a list object that contains all the 'methods' attached to this cachematrix 'object'
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function accessing a cache matrix object that returns the either the inverse of the 
## matrix object or the cached version thereof
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## recall that 'x' is a cachematrix object and therefore to interact with
  ## the data one needs to call one of the getter/setter functions
  s <- x$getsolve()
  if(!is.null(s)) {
    ## shows a message if the result returned is cached
    message("getting cached data")
    return(s)
  }
  ## if no cached result exists, calculate the inverse and store in the cache 's'
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
