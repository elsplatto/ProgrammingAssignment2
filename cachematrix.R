## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates list of functions to
# set  matrix
# get matrix
# set inverted matrix
# get inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function
# cacheSolve inverts the matrix (by calling the solve function)
#in the first part of the function it checks to see if the matrix has already been inverted
#and returns it - if not it sets the inverted matrix

cacheSolve <- function(x, ...) {
  
  m<-x$getmatrix()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
