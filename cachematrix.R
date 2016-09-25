
## The file defines two functions: makeCacheMatrix and cacheSolve
## 
## makeCacheMatrix defines a list for caching the 
##  matrix as well as its cached inverse and the functions
##  used to maintain the cached matrices.
## 
## cacheSolve utilizes the list created by makeCacheMatrix
##  to invert the cached matrix.  
##  If the inverted matrix is already cached, then the cached
##  matrix is returned.  If not, then the cached original, 
##  non-inverted, matrix is inverted, cached and the inverted
##  matrix returned.

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix accepts an invertible matrix (the presumption
  ##  is the matrix is invertible, other use is considered out of
  ##  bounds), and returns a list containing funcations to:
  ##  1: Set the value of the original matrix as well as set the cached 
  ##    inverted matrix to null
  ##  2: Get the original matrix
  ##  3: Set the cached inverted matrix
  ##  4: Get the cached inverted matrix

  inverted <- NULL
  
  set <- function(y) {
    original <<- y
    inverted <<- NULL
  }
  
  get <- function() x
  setInverted <- function(inverted) inverted <<- inverted
  getInverted <- function() inverted
  
  list(set = set, 
       get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}


cacheSolve <- function(x, ...) {
  ## cacheSolve returns the inverse of the matrix stored in the 
  ##  list created by "makeCacheMatrix".  
  ##  
  ##  The presumption is the 
  ##  matrix contained in the list is always invertible and no checks 
  ##  are made to ensure this is the case.
  ##  
  ##  The function first checks to see if the inverse matrix has already 
  ##  been calculated and, if so, returns the cached inverted matrix.  
  ##  
  ##  If the cached inverted matrix does not yet exist, the function inverted the
  ##  matrix using solve(), saves the inverted value, and returns the inverted
  ##  value.  

  inverted <- x$getInverted()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  original <- x$get()
  inverted <- solve(original, ...)
  x$setInverted(inverted)
  inverted
}
