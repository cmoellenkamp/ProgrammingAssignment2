## Put comments here that give an overall description of what your
## functions do

##These two functions create a cache which will first - hold the functions to check if there is a cached solution
##for the inverse of a user defined square matrix and second - compute the inverse of the user defined square matrix
##if there is none cached for that matrix.

## Write a short comment describing this function

##This function specifies the functions that will hold the cached solution of an inverse of a matrix and set the indicators
##and certain variables at a level which can be used within any of these functions.

makeCacheMatrix <- function(x = matrix(nrow=2,ncol=2)) {
 ##initialize cache indicator to NULL or not cached
  m<-NULL
 ##set matrix to be inverted to that entered by user
  set<-function(y){
   ##reset matrix based on "x" matrix entered
    x<<-y
   ##set cache indicator back to NULL to indicate that this is a new matrix
    m<<-NULL
  }
 ##print (or get) the matrix to be inverted
  get<-function() x
 ##set the cache indicator(m) to the matrix's inverse
  setinverse<-function(inv) m<<-inv
 ##print the cached inverse of the current matrix
  getinverse<-function() m
 ##create a list of all four of the functions required to check for a cached value
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

##This function will check for a cached solution for the inverse of a square matrix and will compute the solution
##if it is not cached in which case it will save this solution within the cache and provide it to the users.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 ##set the value of the inverse variable (inv) to the cached inverse if available
  inv<-x$getinverse()
 ##check if the cached value is NULL or "not cached"
  if(!is.null(inv)){
   ##if the inverse is cached print a message notifying user
    message ("getting cached inverse of matrix")
   ##print the value of the inverse matrix to the screen
    return(inv)
  }
 ##if this is a new matrix, set entmatrix to the newly entered matrix
  entmatrix<-x$get()
 ##compute the inverse of the new matrix and save it in inv variable
  inv<-solve(entmatrix,...)
 ##set the inverse cache indicator to the value of the inverse of the matrix
  x$setinverse(inv)
 ##print the newly computed inverse to the screen
  inv
}
