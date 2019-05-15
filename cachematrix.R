## makeCacheMatrix and cacheSolve cache the inverse of a matrix so it can be
## retrieved from the cache when needed instead of repeatedly calculated

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL#set the value of m to NULL
  set<-function(y){
    x<<-y #after set(y),x=y,m=null
    m<<-NULL
  }
  get <-function() x #set the value of get to x
  setinverse<-function(inverse) m<<-inverse() 
  getinverse<-function() m #clone m to getmean(null)
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve checks if the matrix inverse has been cached, and if so returns the
## inverse. If not, it calculates the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
  m<-x$getinverse() #set the value of m to be getmean, if it is not m then the following instructions occur
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }#cached data
  data<-x$get()#set the value of data to be numberic vector
  m<-inverse(data, ...) 
  x$setinverse(m)# cache m
  m
  ## Return a matrix that is the inverse of 'x'
}

