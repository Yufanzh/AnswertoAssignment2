## Put comments here that give an overall description of what your
## functions do
## The pair of function described below can be used to caching the 
## inverse of a matrix to avoid repeatedly matrix inversion (if there are any)


## Write a short comment describing this function
## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
  
}


## Write a short comment describing this function
## The following function calculates the inverse of
## a special "matrix" created with the above function.
## It will first check to see if the inverse has already
## been calculated. If so, it gets the inv value from the
## cache and skips the computation. If not, it will do the
## calculation and sets the inv value in the cache via the 
## setInv function.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
