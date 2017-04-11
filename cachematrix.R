## Fuctions that cache the inverse of a matrix
### This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  # get the value of the matrix  
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(solve) inve <<- solve
  # get the value of the inverse
  getinverse <- function() inve
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


### This function computes the inverse of the special "matrix" returned by 
### makeCacheMatrix above. If the inverse has already been calculated (and 
### the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inve <- x$getinverse()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinverse(inve)
  inve
}