## This assignment asks us to create a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix and cacheSolve are functions that will take a matrix, create a special matrix that will cache the matrix. This then can be taken into the next function
## that will compute the inverse of the special matrix. 

## makeCacheMatrix - This function will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 							##initialize variable
  
  set <- function(y) {		##function "set" - used to set a new matrix
    x <<- y					##matrix x will be updated with a new one 
    inv <<- NULL			##reset inverse back to NULL
  }
  
  get <- function() x			##function "get" - get the current matrix
  
  setInverse <- function(inverse) inv <<- inverse #set the inverse of the matrix
  
  getInverse <- function() inv ##get the inverse of the matrix
  
  list(set = set, get = get,		##return list of functions
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##cacheSolve - function computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse has been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ##retrieve cached matrix
  
  if (!is.null(inv)) {	##if there is a cached matrix, we will return the inverse 
    
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get() ##get the matrix from the cache
  
  inv <- solve(data, ...) ## compute the inverse
  
  x$setInverse(inv) ##cache the inverse
  
  inv ##return the inverse of the matrix 
}
