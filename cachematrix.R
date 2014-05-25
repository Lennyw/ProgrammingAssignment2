## makeCacheMatrix prepares a matrix x to be cached and later processed
## by cacheSolve, which will solve for the inverse of matrix x if it is
## not in the cache created by makeCacheMatrix.

## makeCacheMatrix sets up a function to be performed on a matrix x
## and caches the matrix x, the inverse matrix function we will call, and
## the inverse matrix in a list to be processed by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # This is our matrix
  setsolve <- function(solve) m <<- solve # This will prepare the inverse matrix function for cacheSolve to execute.
  getsolved <- function() m # $getsolved should return m == NULL until cacheSolve is run for the matrix.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolved = getsolved)   
}


## cacheSolve returns the inverse matrix of x from the cache if it exists.
## If this does not exist, it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolved()

  ## The if condition checks to see if the inverse matrix has been calculated for the makeCacheMatrix of our matrix.
  if(!is.null(m)) { 
    message("Retrieving from cache")
    return(m)
  }
  # This part calculates the inverse matrix by retrieving the matrix and 
  # inverse matrix function from the list makeCacheMatrix created.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

