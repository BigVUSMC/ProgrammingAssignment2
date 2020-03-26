## We have functions. 
## The first function is makeCacheMatrix.
## makeCacheMatrix will input a regular matrix
## and outpu a special matrix object that will cache it matrix.


makeCacheMatrix <- function(M = matrix()){
  i <- NULL
  set <- function(x){
    M <<- x
    i <<- NULL
  }
  get <- function() M
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list( set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## CacheSolve will find the inverse of the special matrix
## Created by the makeCacheMatrix.


cacheSolve <- function(M,...){
  i <- M$getinverse()
  if (!is.null(i)){
      message("getting cached data")
      return(i)
  }
  data <- M$get()
  i <- solve(data, ...)
  M$setinverse(i)
}
