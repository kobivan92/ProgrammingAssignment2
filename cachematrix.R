## Programming Assignment 2: Caching the Inverse of a Matrix

## Computing the inverse of a matrix is a costly operation,
## so it is better to cache the result of the computation 
## instead of recalculating it when we need it again

## First function "makeCacheMatrix" gets a squere invertible matrix as input
## and creates a "matrix" object containing input matrix and its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  environment = NULL
  set = function(y) {
    x <<- y
    environment <<- NULL
  }
  get = function() x
  setinverse = function(inverse) environment <<- inverse 
  getinverse = function() environment
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Second function "cacheSolve" computes the inverse if it has not been done before
## and caches the result,
## or it just gets the inverse matrix from cache if it is already there

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is the output of makeCacheMatrix()
  
  inverse = x$getinverse()
  
  ## check if the inverse has been calculated before:
  ## if it has, get it from the cache
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  ## otherwise, calculate the inerse and cache it
  m = x$get()
  inverse = solve(m, ...)
  
  x$setinverse(inverse)
  
  return(inverse)
}

