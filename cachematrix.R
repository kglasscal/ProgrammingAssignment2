## #############################################################################
## These functions store work together to store two matrices, A and its inverse 
## Ainv as part of the current environment. If A is a large matrix, caching it
## will improve the access speed. 
##  
## #############################################################################

## #############################################################################
## The makeCacheMatrix function will store both the A and Ainv as environment 
## variables. It also provides functions to set and get both matrices
## #############################################################################
makeCacheMatrix <- function(A = matrix()) {
  Ainv <- NULL

  set <- function(B) {
    A <<- B
    Ainv <<- NULL
  }
  get <- function() A
  setinverse <- function(B) Ainv <<- B
  getinverse <- function() Ainv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'x'
  Ainv <- A$getinverse()
  if(!is.null(Ainv)) {
    message("getting cached data")
    return(Ainv)
  }
  data <- A$get()
  Ainv <- solve(data, ...)
  A$setinverse(Ainv)
  Ainv 
}
