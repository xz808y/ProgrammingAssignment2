## David Anthony
## 17Feb17
## R Programming class rprog-011

## There are two functions in this R file. makeCacheMatrix()contains functions
## to inverse a matrix and cache its value. This cached value will be retunred to 
## the calling function called cachematrix(). If the matrix changes, a new 
## cached inversed matrix will be created and returned.
 


## makeCacheMatrix() takes a matrix passed to it from cacheSolve() and retuns an 
## inverse of that matrix. It also wil cache this matrix in m and return that
## cached value to allow quick response to the calling function for the 
## inverse if already calculated.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() calles makeCacheMatrix() to calculate the inverse of a matrix or
## return a cached value of the matrix previously calculated.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
