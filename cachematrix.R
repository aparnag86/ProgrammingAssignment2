## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Method to set matrix
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  ## Method to get matrix
  get <- function(){
    m
  }
  
  #Method to set inverse of matrix
  setInverse <- function(inverse){
    i <<- inverse
  }
  ## Method to get inverse of matrix
  getInverse <- function(){
    i
  }
  ## Returns list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Compute inverse of matrix returned by makeCacheMatrix
## If the inverse has already been calculated and
## matrix is not change then cacheSolve should
## retrieve the inverse from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- x$getInverse()
  
  ## Return inverse if its already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Get matrix from object
  data <- x$get()
  ## Calculate inverse using matrix multiplication
  m<- solve(data) %*% data
  ## Set inverse to object
  x$setInverse(m)
  ## Return matrix
  m
}
