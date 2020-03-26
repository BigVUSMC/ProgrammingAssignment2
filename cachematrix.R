## makeCacheMatrix will input a regular matrix M and out a special matrix
## object that will cache it's matrix.

## cacheSolve will find the inverse of the special matrix return by
## makeCacheMatrix

makeCacheMatrix <- function(M = matrix()) {
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


## Write a short comment describing this function
 ## Return a matrix that is the inverse of 'M'

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
        
        

