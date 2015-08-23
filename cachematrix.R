## R Programming Coursera
## Programming Assignment#2


## FUNCTION #1 - "Caching the Inverse of a Matrix"

## The idea is to store a list of values needed for the inverse computation of a matrix.
## The function must allow for Setting and getting values.

## Function names as provided -- "makecacheMatrix()"

makeCacheMatrix <- function(x = matrix()) {
  
  ## need to initialize the variable that will cache the inverse
  invmatrix <- NULL
  
  # specify the "setvalue" of the matrix
  setvalue <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  # specify the "getvalue" of the matrix
  getvalue <- function() x
  
  # set the value of the inverse
  setinv <- function(inv_new) invmatrix <<- inv_new
  
  # get the value of the inverse
  getinv <- function() invmatrix
  
  # return a list of all the above functions
  list(setvalue = setvalue, getvalue = getvalue,
       setinv = setinv,
       getinv = getinv)
  
}


## FUNCTION #2 - "CHECK the CACHE -- Before Computting the Inverse of the Matrix"

## The idea is to check the cache before executing the Inverse Matrix Computation.
## The function uses the solve() command to do the inverse.

## Fucntion names as provided -- "cacheSolve()"

cacheSolve <- function(x, ...) {
  
  ## Getting the inverse value
  invmatrix <- x$getinv()
  
  ##  Checking if the invmatrix == "NULL"?
  ##  Yes, "return(invmatrix)"
  
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  
  ## Computing inverse matrix
  
  ## get the matrix
  data <- x$getvalue()
  
  ## computing the inverse
  inv <- solve(data, ...)
  
  ## After inverse computation, set the cache value
  x$setinv(invmatrix)       
  
  ## return the "inverse matrix"
  invmatrix                 

}
