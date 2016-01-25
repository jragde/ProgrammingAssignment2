## Put comments here that give an overall description of what your
## functions do
##  makeCacheMatrix returns a special "matrix" that is a list of functions
##  cacheSolve calculates inverse of the special "matrix" created using makeCacheMatrix
##
## Write a short comment describing this function
##   This function, makeCacheMatix creates a special "matrix", which is a list containing functions to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
  
  invmtx = NULL
  
  set = function(y) {
    x <<- y
    invmtx <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) invmtx <<- inverse 
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function
## cacheSolve returns the inverse of matrix input to makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  invmtx <- x$getinv()
  
  ## if inverse is already calculated, get from cache  
  if(!is.null(invmtx)) {
    message("getting cached data")
    return(invmtx)
  }
  
  ## if inverse is not already calculated, calculate and return
  data <- x$get()
  invmtx <- solve(data, ...)
  x$setinv(invmtx)
  invmtx  
}
