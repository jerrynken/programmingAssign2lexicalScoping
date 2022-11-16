## Put comments here that give an overall description of what your
## functions do
##This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
##matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-Null
  set<-function(y){
    x<<-y
    m<<-Null
  }
  get<-function()x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m)){
    message("Get cache Data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
