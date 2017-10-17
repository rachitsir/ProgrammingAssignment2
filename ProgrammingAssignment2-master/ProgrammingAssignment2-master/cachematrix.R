# Programming assignment 2

# this function creates a vector cached inverse is to be found 
makeVector <- function(x = matrix()) {
  m <- NULL
  
  set<- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setmean<- function(mean) m <<- mean
  getmean<- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


# this functions returns a matrix that is the inverse of 'x'
cacheMean <- function(x, ...) {

  m <- x$getmean()
  
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
