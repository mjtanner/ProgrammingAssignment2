# Matthew Tanner
# mjtanner@uga.edu
# 770 601-1955

## Put comments here that give an overall description of what your
## functions do
##  These functions create an object that stores a matrix and calculates and 
##  caches the inverse of the matrix. On the first call to cacheSolve(), the 
##  inverse is calculated, cached, and returned. On subsequent calls, the 
##  cached inverse of the matrix is returned.

## Global Variables
##  When test is 'TRUE', the short test sequense at the bottom of the page 
##  is run on sourcing.
test <- FALSE

##  When verbose is 'TRUE', messages indicating whether the inverse matrix is
##  being calculated or the cached inverse matrix is being retrieved are printed
##  tothe console.
verbose <- FALSE


## Write a short comment describing this function
##  This function creates a list of function which, respectively,
##  set the matrix, get the matrix, set the inverse of the matrix and get the
##  inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##  This function calculates the list of functions in the function 
##  "makeCacheMatrix." If the inverse matrix has not been previously calculated
##  it is calculated and returned. Otherwise, if the inverse matrix has 
##  previously been calculated and paired with the variable name "inv", the
##  inverse is not recalculated and, instead, the store matrix is returned.

cacheInverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    if(verbose) message("Getting cached inverse matrix.")
    return(inv)
  }
  matrix <- x$get()
  if(verbose) message("Calling solve().")
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}

## Test
if(test) {
  A = matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3,byrow = TRUE)
  print(A)
  B <- makeCacheMatrix(A)
  C <- cacheInverse(B)
  print(C)
  D <- cacheInverse(B)
  print(D)
  B <- makeCacheMatrix(D)
  E <- cacheInverse(B)
  print(E)
  F <- cacheInverse(B)
  print(F)  
  G <- cacheInverse(B)
  print(G)
}
