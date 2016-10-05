## The purpose of this script is to create functions that work together to 
##   retrieve the inverse of a matrix, calculating and storing that inverse
##   if it has not already been done.

## The makeCacheMatrix function creates the getter and setter functions, 
##   including setsolve and getsolve, which manage the storing and 
##   retrieving of the matrix inverses.  It returns a list of the 
##   individual functions created.

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


## The cacheSolve function receives as input an object of class makeCacheMatrix,
##   checks to see if its inverse of the matrix has been calculated and stored
##   previously, and then either returns the previously calculated inverse or it
##   calculates the inverse, stores it, and returns the inverse.
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
