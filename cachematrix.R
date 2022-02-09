## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
