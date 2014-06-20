## The idea here is to create a matrix which
## contains the matrix and the inverse of itself.
## I am storing the functions itself into the matrix.
## This ensures I get the cached matrix always.

## A special function to create a special matrix which holds both
## an input matrix and its inverse itself.

makeCacheMatrix <- function(x = matrix()) {

  #Variable to hold the inverse of the input matrix.
  inv <- NULL
  
  #This is to set a new input matrix
  #the inv would be reset in this environment.
  setmtx <- function(y){
    x <<- y
    inv <<- NULL
  }  
  
  getmtx <- function() x
  #this is where we cache the inverse of a mtx
  setinv <- function(mtxinv) inv <<- mtxinv    
  getinv <- function()inv   
  
  res <- matrix(list(setmtx,setinv,getmtx,getinv),1,4)
  res #resultant matrix of matrix... :-)
}


## This function takes the special matrix as input 
## and returns the inverse of the matrix data from the cache 
## if its already available otherwise solves the inverse, cache it 
## and then return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x[[1,4]]()  
  if(!is.null(inv)){
    print("getting the inverse from Cache")
    return(inv)
  }
  
  #get the input matrix from the speacial matrix created
  #by the makeCacheMatrix function  
  data <- x[[1,3]]()
  print(data)
  
  #Solve the inverse of the input matrix
  inv <- solve(data)
  x[[1,2]](inv)
  inv
}
