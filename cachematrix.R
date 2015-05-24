## This file contains functions to calculate inverse of a matrix


## makeCacheMatrix is function  which creates list of functions

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(b){
    x<<-b
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) i<<- solve
  getinverse<-function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve function calculates inverse of matrix if not already calculated. 

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix, ...)
  x$setinverse(i)
  i
}
