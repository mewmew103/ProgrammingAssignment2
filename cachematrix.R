## Function to find and cache the inverse of a given square, invertible matrix


## makeCacheMatrix function gets and sets a square invertible matrix whose inverse
## is to be calculated

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
  }


## cacheSolve uses the matrix cached in makeCacheMatrix, checks if inverse has
## already been calculated. If yes, then it returns the cached inverse, if no 
## the inverse is returned after computation using solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

  

