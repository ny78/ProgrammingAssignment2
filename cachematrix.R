## Put comments here that give an overall description of what your
## functions do 

## This function creates a special "matrix" object that can cache its inverse.
## A square invertible matrix is passed to the makeCacheMatrix function
## To set and get the value of the matrix 
## To set and get the inverse data of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function verifies if the inverse of the matrix has been computed.  
## If true, it returns the message "getting inverse matrix cached data"
## and the result of the inverse matrix.
## Else, it computes the inverse of the matrix by calling the makeCacheMatrix
## function to set and get the inverse of the matrix.  Then it calls the 
## setinverse function to cache to matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting inverse matrix cached data")
    return(m)
  }
  
  my_data <- x$get()
  m <- solve(my_data, ...)
  x$setinverse(m)
  m
}
