# Program to cache matrix inversion


# Matrix inversion is  a costly computation usually 
# A solution is to cache the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  
  set<- function(y){  # sets the value of the matrix
    x<<-y
    inverse_value=NULL
  }
  get <- function() x # returns the value of the matrix
  
  setinv<- function(inverse) inverse_value <<- inverse # sets the value of the inverse matrix
  getinv<- function() inverse_value # returns the value of the inverse matrix
  
  list(set=set, get=get, setinv=setinv, getinv= getinv)
}

# this functions Return a matrix that is the inverse of 'x'
# firstly it checks the inverse has already been computed.
#If inverse has been already completed it displays the result and skips the computation.
# Else, it computes the inverse and sets the value in the cache using setinverse function.


cacheSolve <- function(x, ...) {

  inverse_value<- x$getinv()
  
  if(!is.null(inverse_value)){
    message('Inverse Value is Cached ')
    return(inverse_value)
  }
  
  y<- x$get()
  inverse_value<= solve(y, ...)
  
  x$setinv(inverse_value)
  
  return(inverse_value)
}
