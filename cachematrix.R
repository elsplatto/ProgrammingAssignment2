## Put comments here that give an overall description of what your
## functions do

##Acknowledgements & Disclosure
#This code heavily replicates code I have seen while finding help for this assignment. Namely from
#stackoverflow.com (http://stackoverflow.com/questions/23796316/returning-the-inverse-matrix-from-a-cached-object-in-r)
#I have re-written it in my own hand but once learned something can not be unlearned and I am aware
#my code moves into close plagerism territory. 

#I would hope that my commenting shows that I understand the workings of the code intimately.

##=========makeCacheMatrix=================
#The makeCacheMatrix function is a 'parent' function that contains a list of functions 
#that do the grunt work 
#It sets and gets the matrix in both it's original and inverted forms
#It doesn't actually invert the matrix - the onus of that is handled in 
#cacheSolve

##=========cacheSolve=================
#The cacheSolve function inverts the matrix
#It also checks to see if the matrix has already been inverted or not
#and returns the cached inverted matrix if it has already been computed
#or inverts the matrix and returns it if it has not yet been inverted


##Once the makeCacheMatrix has been assigned you can call it's child functions using the $ syntax

##Eg. 
#a<-makeCacheMatrix()
#a$set(matrix(1:4,2,2))
#a$get()
#returns the following - matrix in it's original form
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

#to invert and set do the following
#cacheSolve(a)
#returning inverted matrix
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## Write a short comment describing this function

#makeCacheMatrix creates list of functions to
# set matrix
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
#in the first part of the function it checks to see if the inverted matrix has already been inverted
#and returns it - if not it sets the inverted matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  if(!is.null(m)){
    #getting cached matrix
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
