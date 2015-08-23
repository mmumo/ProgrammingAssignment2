## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##1. this function will set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##2. This function will return the value of the matrix
  get <- function() x
  
  ##3. This funciton will set the inverse of the matrix
  setinverse <- function(inv) m <<- inv
  
  ##4. This function will return the inverse of the function
  getinverse <- function() m
  
  ##the list function returns all the 4 above functions as a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve function will take a matrix x and return it's inverse by
## running the solve(x) function which returns the inverse of a matrix
## we will assume that the matrix must be square

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  
      ##get the special matrix ( from makeCacheMatrix) and get the stored inverse matrix.
      ## if it doesn't exist, then calculate the inverse
  
      m <- x$getinverse()
      if(!is.null(m)) {
        ## the inverse exists in the cache, so just return the inverse.
        message("getting cached data")
        return(m)
      }
      
      ## the inverse is not in the cache, so calculate the inverse using the solve(x) function
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
