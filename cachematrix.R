## Function: makeCacheMatrix: Creates a "special matrix".
## set the value of the matrix
## get the value of the matrix
## set the mean value of the matrix
## get the mean value of the matrix

## Function: cacheSolve: Gets the Inverse of the "special matrix".

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse.  
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

## Function: cacheSolve: Gets the Inverse of the "special matrix".

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  
  ##This function computes the inverse of the special "matrix" returned by
  ##makeCacheMatrix above. If the inverse has already been calculated (and the 
  ##matrix has not changed), then the cachesolve should retrieve the inverse 
  ##from the cache.
  
  ##Computing the inverse of a square matrix can be done with the solve 
  ##function in R. For example, if X is a square invertible matrix, then 
  ##solve(X) returns its inverse.
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