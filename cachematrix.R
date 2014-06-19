## The idea here is to matri

## A special function to create a special matrix which holds both
## an input matrix and its inverse itself.

makeCacheMatrix <- function(x = matrix()) {

  #Variable to hold the inverse of the input matrix.
  inv <- NULL
  #whenever the makeCacheMatrix is called for a new matrix
  #the inv should be reset
  setmtx <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmtx <- function() x
  #this is where we cache the inverse of a mtx
  setinv <- function(mtxinv) inv <<- mtxinv
  #simply returns the cached inverse of a matrix.
  getinv <- function() inv
  
  matrix(list(x,inv)) #resultant matrix of matrix... :-)
}


## This function takes the special matrix as input 
## and returns the inverse of the matrix data from the cache 
## if its already available otherwise solves the inverse, cache it 
## and then return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    print("getting the inverse from Cache")
    return(inv)
  }
  
  #get the input matrix from the speacial matrix created
  #by the makeCacheMatrix function
  data <- x$get()
  
  #Solve the inverse of the input matrix
  inv <- solve(data)
  x$setinv(inv)
  inv
}
