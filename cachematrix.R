## Coursera R programming Course Assignment-2. Student author: codetravel Apr-2014.
## Define a set of functions to store a matrix, calculate its inverse and cache it.

## makeCacheMatrix creates an object that caches a matrix and it's inverse
##         and methords to set and get these objects
## makeCacheMatrix has 2 properties 'x' and it's inverse 'iX' (both matrixes) 
##                 and 4 methods set(), get(), setMatrixInverse(), getMatrixInverse()  
makeCacheMatrix <- function(x = matrix()) {
  ##   note: makeCacheMatrix(input_parm) sets 'x' to the value input_parm
  
  iX <- NULL
  
  ## set() method to set the value of 'x'
  set <- function(y) {
    x <<- y       ## if a value 'y' is passed it gets set to 'x'
    iX <<- NULL   ## if 'x' is new then reset it's inverse 'iX' to NULL
  }
  
  ## get() method
  get <- function() x  ## return 'x'
  
  ## setMatrixInverse() method
  setMatrixInverse <- function(y) {
    iX <<- y           ## if a value 'y' is passed it gets set to 'iX'
  }
  
  ## getMatrixInverse() method
  getMatrixInverse <- function() iX  ## return inverse metrix 'iX'
  
  ## makeCacheMatrix() function returns an object 'list' with 4 methods
  list(set=set, get=get, setMatrixInverse=setMatrixInverse, 
       getMatrixInverse=getMatrixInverse)
  
}


## cacheSolve is a function that return the inverse of an object 'x' matrix.
## if the inverse is already cached in the object 'x' then it is returned without recalculating
## if the inverse is not cached it is calculated, returned and also cached within the object 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix 'iX' that is the inverse of 'x'
  
  ## retrive matrix inverse if it is already calculated and cached
  iX <- x$getMatrixInverse()
  if(!is.null(iX)){
    message("getting cache data")
    return(iX)
  }
  
  ## Otherwise calculates the inverse of 'x', caches, and returns it
  iX <- solve(x$get())    ## retrieve x and calc it's inverse 
  x$setMatrixInverse(iX)  ## cache the inverse 'iX' as a property of 'x'
  return(iX)
}
